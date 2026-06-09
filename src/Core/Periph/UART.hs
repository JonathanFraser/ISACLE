module Core.Periph.UART
    ( UARTRegs(..)
    , uartUnit
    ) where

import Clash.Prelude

-- | Memory-mapped UART registers.
--
--   Register layout relative to base address:
--     base + 0  UDR   read/write  data register (Rx buffer on read, Tx buffer on write)
--     base + 1  USR   read-only   status (bit 0 = UDRE: Tx empty, bit 1 = RXC: Rx complete)
--     base + 2  UBRR  read/write  baud rate divisor (system clock cycles per baud period)
data UARTRegs dat = UARTRegs
    { udr  :: dat           -- Tx shift register / Rx buffer
    , ubrr :: dat           -- baud rate divisor
    } deriving (Generic, NFDataX, Show, Eq)

-- | UART Tx state machine.
data TxState dat
    = TxIdle
    | TxStart  dat (Unsigned 16)        -- (data byte, baud counter)
    | TxBit    dat (Unsigned 4) (Unsigned 16)  -- (shift reg, bits remaining, baud counter)
    | TxStop   (Unsigned 16)            -- baud counter
    deriving (Generic, NFDataX, Show, Eq)

-- | UART Rx state machine.
--   The shift register is always a raw 'BitVector 8'; conversion to @dat@
--   happens only on completion, so no 'Bits' constraint is needed on @dat@.
data RxState dat
    = RxIdle
    | RxStart  (Unsigned 16)                         -- baud counter
    | RxBit    (BitVector 8) (Unsigned 4) (Unsigned 16) -- (shift reg, bits received, ctr)
    | RxDone   dat
    deriving (Generic, NFDataX, Show, Eq)

data UARTState dat = UARTState
    { txState :: TxState dat
    , rxState :: RxState dat
    , txBuf   :: Maybe dat              -- pending Tx byte (written, not yet started)
    , rxBuf   :: Maybe dat              -- received byte (unread)
    , uartBrr :: Unsigned 16           -- baud rate divisor (runtime)
    } deriving (Generic, NFDataX, Show)

-- | Generic memory-mapped 8N1 UART.
--
--   @dat@ must be exactly 8 bits wide for correct 8N1 framing; use
--   @Unsigned 8@ or @BitVector 8@.
--
--   The baud rate divisor is stored in the UBRR register and sets the number
--   of system clock cycles per baud period.  For example, at 50 MHz with
--   115200 baud, set UBRR = 434.
--
--   Status flags (USR):
--     bit 0  UDRE  Tx data register empty — safe to write a new byte
--     bit 1  RXC   Rx complete — a received byte is waiting in UDR
--
--   Register layout:
--     base + 0  UDR   data register
--     base + 1  USR   status (read-only; writes ignored)
--     base + 2  UBRR  baud divisor (low byte; runtime configurable)
uartUnit
    :: forall dom addr dat
     . ( HiddenClockResetEnable dom
       , NFDataX dat, Num dat, Bits dat, BitPack dat, BitSize dat ~ 8
       , Eq addr, Num addr
       )
    => addr                                      -- base address
    -> Unsigned 16                               -- initial baud divisor (overridable via UBRR)
    -> Signal dom Bool                           -- Rx line (serial input)
    -> Signal dom (Maybe addr)                   -- bus read address
    -> Signal dom (Maybe (addr, dat))            -- bus write
    -> ( Signal dom dat                          -- read data
       , Signal dom Bool                         -- Tx line (serial output)
       , Signal dom Bool                         -- Rx complete interrupt
       , Signal dom Bool                         -- Tx empty interrupt (UDRE)
       )
uartUnit base initBrr rxLine rdAddr wr = (rdData, txLine, rxIrq, txIrq)
  where
    initState = UARTState TxIdle RxIdle Nothing Nothing initBrr

    step st (rxBit, mrd, mwr) =
        let brr = uartBrr st

            -- ── Handle bus writes ──────────────────────────────────────────
            st1 = case mwr of
                Just (a, v)
                    | a == base     -> st { txBuf   = Just v }
                    | a == base + 2 -> st { uartBrr = unpack (resize (pack v)) }
                _                   -> st

            -- ── Advance Tx state machine ───────────────────────────────────
            -- Feed the PRE-write txBuf so the Tx starts one cycle after the
            -- CPU writes UDR.  This gives UDRE one visible low cycle (the
            -- write cycle) before rising again when Tx picks up the byte.
            (txSt', txBit, txStarted) = stepTx (txState st1) (txBuf st) brr

            -- Clear txBuf if Tx just started consuming it (based on old buf)
            st2 = st1 { txState = txSt'
                      , txBuf   = if txStarted then Nothing else txBuf st1 }

            -- ── Advance Rx state machine ───────────────────────────────────
            (rxSt', mRxByte) = stepRx (rxState st2) rxBit brr

            -- Latch completed Rx byte (newest wins if overrun)
            newRxBuf = case mRxByte of
                Just b  -> Just b
                Nothing -> rxBuf st2

            st3 = st2 { rxState = rxSt', rxBuf = newRxBuf }

            -- ── Bus read ───────────────────────────────────────────────────
            udre = case txBuf st3 of Nothing -> True; _ -> False
            rxc  = case rxBuf st3 of Just _  -> True; _ -> False

            status :: dat
            status = (if udre then 1 else 0) .|. (if rxc then 2 else 0)

            -- Reading UDR clears the Rx buffer
            (rdVal, st4) = case mrd of
                Just a
                    | a == base     ->
                        let b = maybe 0 id (rxBuf st3)
                        in (b, st3 { rxBuf = Nothing })
                    | a == base + 1 -> (status, st3)
                    | a == base + 2 -> (fromIntegral (uartBrr st3), st3)
                _                   -> (0, st3)

        in (st4, (rdVal, txBit, rxc, udre))

    out    = mealy step initState (bundle (rxLine, rdAddr, wr))
    rdData = fmap (\(r, _, _, _) -> r) out
    txLine = fmap (\(_, t, _, _) -> t) out
    rxIrq  = fmap (\(_, _, r, _) -> r) out
    txIrq  = fmap (\(_, _, _, t) -> t) out

-- ---------------------------------------------------------------------------
-- Tx state machine (pure, called each cycle)
-- ---------------------------------------------------------------------------

stepTx :: (Num dat, BitPack dat, BitSize dat ~ 8)
       => TxState dat -> Maybe dat -> Unsigned 16
       -> (TxState dat, Bool, Bool)
                          -- (next state, tx line level, consumed txBuf)
stepTx TxIdle (Just byte) _   = (TxStart byte 0, True, True)
stepTx TxIdle Nothing     _   = (TxIdle,         True, False)

stepTx (TxStart byte ctr) _ brr
    | ctr + 1 >= brr = (TxBit byte 0 0,      False, False)
    | otherwise      = (TxStart byte (ctr+1), False, False)

stepTx (TxBit byte bitN ctr) _ brr
    | ctr + 1 >= brr =
        let bitVal  = testBit (pack byte) (fromIntegral bitN)
            bitN'   = bitN + 1
            nextSt  = if bitN' >= 8 then TxStop 0 else TxBit byte bitN' 0
        in (nextSt, bitVal, False)
    | otherwise =
        let bitVal = testBit (pack byte) (fromIntegral bitN)
        in (TxBit byte bitN (ctr + 1), bitVal, False)

stepTx (TxStop ctr) _ brr
    | ctr + 1 >= brr = (TxIdle,          True, False)
    | otherwise      = (TxStop (ctr + 1), True, False)

-- ---------------------------------------------------------------------------
-- Rx state machine (pure, called each cycle)
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
        let acc'  = if rxBit
                    then acc .|. (1 `shiftL` fromIntegral bitN)
                    else acc
            bitN' = bitN + 1
        in if bitN' >= 8
           then (RxIdle, Just (unpack acc'))
           else (RxBit acc' bitN' 0, Nothing)
    | otherwise = (RxBit acc bitN (ctr + 1), Nothing)

stepRx (RxDone byte) _ _ = (RxIdle, Just byte)
