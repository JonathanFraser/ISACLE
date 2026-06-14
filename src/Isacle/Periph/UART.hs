module Isacle.Periph.UART
    ( -- * Peripheral kind tag
      UART
      -- * PeriphDef description (single source of truth)
    , uartDef
    , uartSpecPeriphDef
      -- * Physical I/O instance
      -- (instance HasPhysIO UART)
      -- * Backward-compatible circuit wrapper
    , uartUnit
    ) where

import Clash.Prelude hiding (register)
import Data.Word (Word8)

import Isacle.System.Periph
import Isacle.System.Circuit
import Isacle.System.Spec (NullSig(..))

-- ---------------------------------------------------------------------------
-- Peripheral kind tag
-- ---------------------------------------------------------------------------

data UART

-- ---------------------------------------------------------------------------
-- Internal state types (serial FSM)
-- ---------------------------------------------------------------------------

data TxState dat
    = TxIdle
    | TxStart  dat (Unsigned 16)
    | TxBit    dat (Unsigned 4) (Unsigned 16)
    | TxStop   (Unsigned 16)
    deriving (Generic, NFDataX, Show, Eq)

data RxState dat
    = RxIdle
    | RxStart  (Unsigned 16)
    | RxBit    (BitVector 8) (Unsigned 4) (Unsigned 16)
    | RxDone   dat
    deriving (Generic, NFDataX, Show, Eq)

data SerialState dat = SerialState
    { ssTxState :: TxState dat
    , ssRxState :: RxState dat
    , ssTxBuf   :: Maybe dat
    , ssRxBuf   :: Maybe dat
    } deriving (Generic, NFDataX, Show)

-- ---------------------------------------------------------------------------
-- Register map — single source of truth
-- ---------------------------------------------------------------------------

-- | UART register map.
--
--   offset 0  UDR   read/write  TX write / RX read (reads return Rx buffer)
--   offset 1  USR   read-only   status (bit 0 = UDRE, bit 1 = RXC)
--   offset 2  UBRR  read/write  baud rate divisor
--
-- @stat@ and @rxData@ are driven by the serial state machine.
-- Returns @(txData, txStrobe, baud)@ for the serial FSM.
-- @txStrobe@ pulses True on the cycle the CPU writes to UDR.
uartDef
    :: (Applicative sig, Num dat)
    => sig dat    -- ^ status register value (driven by serial FSM)
    -> sig dat    -- ^ RX buffer (read side of UDR, driven by serial FSM)
    -> PeriphDef UART sig dat (sig dat, sig Bool, sig dat)
uartDef stat rxData = do
    field8 ReadWrite 0 "UDR" "TX write / RX read (reads return Rx buffer)"
    (txData, txStrobe) <- onWriteStrobe 0 0
    onRead 0 rxData                -- reads RX buffer, not TX register

    register RW8 1 "USR" "Status"
        [ bitF ReadOnly 0 "UDRE" "TX data register empty (safe to write)"
        , bitF ReadOnly 1 "RXC"  "RX complete (received byte ready)"
        ]
    onRead 1 stat                  -- read-only: no onWrite

    field8 ReadWrite 2 "UBRR" "Baud rate divisor (system clocks per baud period)"
    baud <- onWrite 2 0
    onRead 2 baud

    return (txData, txStrobe, baud)

-- | Spec-mode 'PeriphDef' for use with 'mkPeriph'.
--
-- Accepts the RX pin input (for type compatibility with the synthesis path)
-- but uses stub signals in the spec interpreter.  Returns @(TX, rxIrq, txIrq)@
-- as stubs; the physical pins should be wired with 'externalIn'\/'externalOut'
-- at the 'SystemDSL' level.
--
-- For actual synthesis circuits use 'uartUnit' directly.
uartSpecPeriphDef
    :: Applicative sig
    => sig Bool   -- ^ RX serial line (unused in spec mode)
    -> PeriphDef UART sig (BitVector 8) (PhysOutputs UART sig)
uartSpecPeriphDef _rxPin =
    uartDef (pure 0) (pure 0) >> return (pure False, pure False, pure False)

-- ---------------------------------------------------------------------------
-- Physical I/O instance
-- ---------------------------------------------------------------------------

instance HasPhysIO UART where
    type PeriphSize  UART     = 3   -- UDR(0), USR(1), UBRR(2)
    type PhysInputs  UART sig = sig Bool
    type PhysOutputs UART sig = (sig Bool, sig Bool, sig Bool)  -- (TX, rxIrq, txIrq)
    nullOutputs _ = (NullSig, NullSig, NullSig)

-- ---------------------------------------------------------------------------
-- Serial state machine (synthesis only)
-- ---------------------------------------------------------------------------

serialFSM
    :: ( HiddenClockResetEnable dom
       , NFDataX dat, Num dat, Bits dat, BitPack dat, BitSize dat ~ 8
       )
    => Unsigned 16                          -- ^ initial baud divisor
    -> Signal dom dat                       -- ^ baud register (from uartDef)
    -> Signal dom dat                       -- ^ txData: byte to transmit (from uartDef)
    -> Signal dom Bool                      -- ^ txStrobe: True when CPU writes UDR
    -> Signal dom Bool                      -- ^ UDR read strobe (clears RX buffer)
    -> Signal dom Bool                      -- ^ RX serial line
    -> ( Signal dom Bool                    -- ^ TX serial line
       , Signal dom dat                     -- ^ status register value
       , Signal dom dat                     -- ^ RX buffer (read side of UDR)
       , Signal dom Bool                    -- ^ RX complete IRQ
       , Signal dom Bool                    -- ^ TX empty (UDRE) IRQ
       )
serialFSM initBrr baud txData txStrobe udrRead rxLine =
    let initSt = SerialState TxIdle RxIdle Nothing Nothing

        step st (rxBit, txStr, txDat, brrVal, udrRd) =
            let brr = unpack (resize (pack brrVal)) :: Unsigned 16

                -- Latch TX byte into buffer on write strobe
                st1 = if txStr then st { ssTxBuf = Just txDat } else st

                -- TX FSM runs on PRE-write txBuf (so TX starts one cycle
                -- after the CPU writes, giving UDRE one visible-low cycle)
                (txSt', txBit, txConsumed) = stepTx (ssTxState st) (ssTxBuf st) brr
                st2 = st1 { ssTxState = txSt'
                           , ssTxBuf   = if txConsumed then Nothing else ssTxBuf st1 }

                -- Advance RX FSM
                (rxSt', mRxByte) = stepRx (ssRxState st2) rxBit brr
                st3 = st2 { ssRxState = rxSt'
                           , ssRxBuf   = case mRxByte of
                                             Just b  -> Just b
                                             Nothing -> ssRxBuf st2 }

                -- Clear RX buffer when CPU reads UDR
                st4 = if udrRd then st3 { ssRxBuf = Nothing } else st3

                udre   = case ssTxBuf st4 of Nothing -> True; _ -> False
                rxc    = case ssRxBuf st4 of Just _  -> True; _ -> False
                statV  = (if udre then 1 else 0) .|. (if rxc then 2 else 0)
                rxBufV = maybe 0 id (ssRxBuf st4)

            in (st4, (txBit, statV, rxBufV, rxc, udre))

        out    = mealy step initSt (bundle (rxLine, txStrobe, txData, baud, udrRead))
        txLine = fmap (\(t, _, _, _, _) -> t) out
        statS  = fmap (\(_, s, _, _, _) -> s) out
        rxBufS = fmap (\(_, _, r, _, _) -> r) out
        rxIrq  = fmap (\(_, _, _, r, _) -> r) out
        txIrq  = fmap (\(_, _, _, _, t) -> t) out
    in (txLine, statS, rxBufS, rxIrq, txIrq)

-- ---------------------------------------------------------------------------
-- TX state machine
-- ---------------------------------------------------------------------------

stepTx :: (Num dat, BitPack dat, BitSize dat ~ 8)
       => TxState dat -> Maybe dat -> Unsigned 16
       -> (TxState dat, Bool, Bool)
stepTx TxIdle (Just byte) _   = (TxStart byte 0, True, True)
stepTx TxIdle Nothing     _   = (TxIdle,         True, False)

stepTx (TxStart byte ctr) _ brr
    | ctr + 1 >= brr = (TxBit byte 0 0,      False, False)
    | otherwise      = (TxStart byte (ctr+1), False, False)

stepTx (TxBit byte bitN ctr) _ brr
    | ctr + 1 >= brr =
        let bitVal = testBit (pack byte) (fromIntegral bitN)
            bitN'  = bitN + 1
        in (if bitN' >= 8 then TxStop 0 else TxBit byte bitN' 0, bitVal, False)
    | otherwise =
        (TxBit byte bitN (ctr+1), testBit (pack byte) (fromIntegral bitN), False)

stepTx (TxStop ctr) _ brr
    | ctr + 1 >= brr = (TxIdle,           True, False)
    | otherwise      = (TxStop (ctr + 1), True, False)

-- ---------------------------------------------------------------------------
-- RX state machine
-- ---------------------------------------------------------------------------

stepRx :: (BitPack dat, BitSize dat ~ 8)
       => RxState dat -> Bool -> Unsigned 16
       -> (RxState dat, Maybe dat)
stepRx RxIdle rxBit _
    | not rxBit = (RxStart 0, Nothing)
    | otherwise = (RxIdle,    Nothing)

stepRx (RxStart ctr) _ brr
    | ctr + 1 >= brr `shiftR` 1 = (RxBit 0 0 0, Nothing)
    | otherwise                  = (RxStart (ctr + 1), Nothing)

stepRx (RxBit acc bitN ctr) rxBit brr
    | ctr + 1 >= brr =
        let acc'  = if rxBit then acc .|. (1 `shiftL` fromIntegral bitN) else acc
            bitN' = bitN + 1
        in if bitN' >= 8
           then (RxIdle, Just (unpack acc'))
           else (RxBit acc' bitN' 0, Nothing)
    | otherwise = (RxBit acc bitN (ctr + 1), Nothing)

stepRx (RxDone byte) _ _ = (RxIdle, Just byte)

-- ---------------------------------------------------------------------------
-- Backward-compatible circuit wrapper
-- ---------------------------------------------------------------------------

-- | Generic memory-mapped 8N1 UART, derived from 'uartDef' + 'serialFSM'.
--
--   Register layout:
--     base + 0  UDR   data register (TX on write, RX on read)
--     base + 1  USR   status (read-only)
--     base + 2  UBRR  baud rate divisor
uartUnit
    :: forall dom addr dat.
       ( HiddenClockResetEnable dom
       , Integral addr, Num addr, Eq addr
       , NFDataX dat, Num dat, Bits dat, BitPack dat, BitSize dat ~ 8
       )
    => addr                               -- ^ base address
    -> Unsigned 16                        -- ^ initial baud divisor
    -> Signal dom Bool                    -- ^ RX serial line
    -> Signal dom (Maybe addr)            -- ^ bus read address
    -> Signal dom (Maybe (addr, dat))     -- ^ bus write
    -> ( Signal dom dat                   -- ^ read data
       , Signal dom Bool                  -- ^ TX serial line
       , Signal dom Bool                  -- ^ RX complete IRQ
       , Signal dom Bool                  -- ^ TX empty (UDRE) IRQ
       )
uartUnit base initBrr rxLine rdAddr wr = (rdData, txLine, rxIrq, txIrq)
  where
    -- UDR read strobe: True when CPU reads offset 0
    udrRead :: Signal dom Bool
    udrRead = fmap isUdrRead rdAddr
      where
        isUdrRead Nothing  = False
        isUdrRead (Just a) = fromIntegral (a - base) == (0 :: Word8)

    -- Mutual recursion (broken by mealy registers in serialFSM):
    --   stat, rxBuf ← serialFSM ← uartDef ← stat, rxBuf
    ((txData, txStrobe, baud), rdData) =
        runSynthPeriph base wr rdAddr (uartDef stat rxBuf)

    (txLine, stat, rxBuf, rxIrq, txIrq) =
        serialFSM initBrr baud txData txStrobe udrRead rxLine
