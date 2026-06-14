# ISACLE Architecture

ISACLE is a SoC assembly kit for Clash-based hardware designs. It provides the DSL, bus model, peripheral abstraction, and synthesis runner. CPU architectures and peripherals specific to a given ISA live in separate consumer packages that depend on ISACLE.

## Consumer packages

| Package | ISA / role |
|---------|-----------|
| **clavr** | AVR 8-bit — CPU core, AVR peripherals (UART, timer, GPIO), example SoC |
| **cl51** | 8051 — deferred |

A consumer imports `Core.System.*` from ISACLE and provides: a `HarvardCPU` instance, any ISA-specific peripherals, and a top-level SoC description written against the `SystemDSL` typeclass.

---

## Goals

1. **Single source of truth per peripheral.** A peripheral's register map, signal behaviour, and documentation metadata all come from one `PeriphDef` value. Running it under different interpreters extracts whatever aspect is needed.

2. **Type-safe bus topology.** `BusDef bus a` is a structural description of an address space expressed in the same `do`-notation the user writes for hardware. Addresses are constants, not runtime values.

3. **Two clean interpreter paths, zero coupling.**
   - *Spec path* (`SpecWriter` / `NullSig`): pure Haskell, produces `SystemSpec` (memory map, port list, register definitions). No signals ever touch this path.
   - *Synthesis path* (`Synth dom` / `Signal dom`): Clash-synthesisable, produces VHDL/Verilog. No `NullSig` ever touches this path.

4. **Clash synthesis must terminate on realistic circuits** without raising the default inline/spec limits.

---

## Core abstractions

### `PeriphDef p sig dat a`

A monadic description of one peripheral:

```
PeriphDef GPIO sig (BitVector 8) (sig (BitVector 8), sig (BitVector 8))
```

- `p` — phantom peripheral kind tag (e.g. `GPIO`, `UART`).
- `sig` — signal functor: `Signal dom` for synthesis, `NullSig` for spec.
- `dat` — bus data word (e.g. `BitVector 8`).
- `a` — physical I/O outputs (port signals, IRQ lines, …).

Operations inside a `PeriphDef`:
- `onWrite offset initVal` — create a read/write register; returns the write-data signal.
- `onRead offset sig` — attach `sig` as the read-data source for an offset.
- `field` / `field8` / `register` — declare metadata (name, access mode, width) for the spec path.

`HasPhysIO` relates a peripheral kind to its physical I/O type family:

```haskell
class HasPhysIO p where
    type PhysInputs  p sig
    type PhysOutputs p sig
```

---

### `BusDef bus a`

A Writer-style product type for bus topology:

```haskell
data BusDef bus a = BusDef [ComponentSpec] a
```

Not a State monad — `attach`, `label`, and `>>=` are pure list transformations. `runBusDef` is a single pattern match with no monad machinery visible to Clash.

| Operation | Purpose |
|-----------|---------|
| `attach base inner` | Shift all component addresses in `inner` by `base`. |
| `periph bp` | Lift a wired peripheral handle into a BusDef token at offset 0. |
| `ramSegment sz sid` | RAM region token. |
| `romBlock sz` | ROM region token. |
| `label @"name" inner` | Tag components with a section name (linker / docs). |

Addresses are **fully resolved at BusDef construction time** — they are constants in the `ComponentSpec` list, not computed at synthesis time.

---

### `SystemDSL m sig dat`

The typeclass consumers write against:

```haskell
class (Monad m, Applicative sig) => SystemDSL m sig dat | m -> sig dat where
    mkPeriph   :: HasPhysIO p => PeriphDef p sig dat (PhysOutputs p sig)
               -> m (BusDef bus (), PhysOutputs p sig)
    mkRamP     :: KnownNat n => Proxy n -> m (BusDef bus ())
    mkRomP     :: ... => Proxy capacity -> Vec n (BitVector 16) -> m (BusDef bus ())
    harvestCPU :: HarvardCPU cpu => cpu -> BusDef () () -> BusDef () () -> IrqDef sig -> m ()
```

The functional dependency `m -> sig dat` means the monad uniquely determines the signal type and data word — GHC resolves everything without ambiguity. The same SoC description (e.g. `avrDataBus`) compiles under both interpreters with no changes.

---

## Interpreter: `SpecWriter`

```
SystemDSL SpecWriter NullSig (BitVector 8)
```

- Accumulates `ComponentSpec` entries into `SystemSpec`.
- `NullSig` carries no runtime value; all signal expressions are `NullSig`.
- `runSpecWriter` returns `(result, SystemSpec)`.
- Used by documentation generators, linker-script tools, and address-map utilities (`specAddrList`, `specAddrMap`).

**Invariant:** `SpecWriter` and `Synth dom` share zero code. `specAddrList` and `runSynth` are completely independent.

---

## Interpreter: `Synth dom`

```
SystemDSL (Synth dom) (Signal dom) (BitVector 8)
```

### Monad representation

```haskell
newtype Synth dom a = Synth
    { unSynth :: SynthEnv dom -> SynthSt dom -> (a, SynthSt dom) }
```

**Flat Reader+State** — one function type, no nested transformer. Each `>>=` produces exactly one GHC Core lambda, so Clash's InlineNonRep pass inlines it once per bind site (vs. twice for a layered `Reader (State s)` design).

### State split: pure vs. signal

`SynthSt dom` separates pure bookkeeping from `Signal` fields:

```
SynthSt dom
├── stPure :: SynthPureSt          -- no Signal fields
│   ├── spPeriphCount  :: Int
│   ├── spFinalBases   :: IntMap Word32
│   └── spDataBusSpecs :: [ComponentSpec]
├── stRdData  :: Signal dom (BitVector 8)
├── stRom     :: Maybe (Signal dom ... -> Signal dom ...)
└── stCPUBind :: Maybe (Signal dom ... -> ...)
```

Clash's InlineNonRep can project `SynthSt → SynthPureSt → spDataBusSpecs` without touching any `Signal` expression. This lets the address map be extracted after pass 1 without Clash confusing the spec list with the signal-level circuit.

### Two-pass address resolution

Peripherals need their base address to build address-decoder logic (`addr >= base && addr < base + size`). The base address is set by `attach` calls inside the `dataBus` value passed to `harvestCPU` — the last step in the do-block. So it is only known after the full monad chain has run.

| Pass | Env | Purpose |
|------|-----|---------|
| 1 (dummy) | `pure Nothing` signals | Run the full monad; extract `spDataBusSpecs` from `stPure dummyFinalSt`; build `addrMap :: IntMap Word32`. |
| 2 (real) | real `Signal` env | Run with `spFinalBases = addrMap`; Clash evaluates `IM.lookup n addrMap` to a concrete `Word32` constant folded into comparator logic. |

The Signal-level feedback loop (CPU rdData → wrBus/rdAddr → peripherals → rdData) is a lazy knot at the `Signal dom` level via `SynthEnv`. It does not pass through `SynthSt`, so Clash's InlineNonRep terminates cleanly.

### What must NOT happen during synthesis

- **No `NullSig`** invocation or consumption at any point, including via Template Haskell splices.
- **No runtime address lookup** — `IM.lookup n addrMap` is evaluated by Clash's partial evaluator to a concrete `Word32` constant baked into combinational logic. It is not a hardware multiplexer.
- **No coupling** between `specAddrList` / `SpecWriter` and `runSynth` / `Synth dom`.

---

## What `attach` means in hardware

```haskell
let dataBus = do
    attach 0x0040 uartBus
    attach 0x0200 sram
```

`attach` shifts the `ComponentSpec` base address in the `BusDef` product — it is a pure list transformation. The address `0x0040` becomes a constant in the spec list.

`harvestCPU` calls `runBusDef dataBus` to extract those constants into the address map. In pass 2, each peripheral's decoder logic becomes:

```
wrEnable ≡ (addr >= 0x0040) && (addr < 0x0043)
```

Pure combinational decoding, no runtime table.

---

## Intended future direction: Bus Master object

`harvestCPU` currently adds one monad bind to the chain and forces the two-pass design (addresses known last, needed first). A cleaner approach:

1. `mkPeriph` / `mkRamP` / `mkRomP` return BusDef tokens as before, shrinking the monad chain to N peripheral binds.
2. The consumer assembles `dataBus :: BusDef () ()` outside the monad — at that point all addresses are already baked into the BusDef product.
3. `runSynth` takes the assembled `BusDef` directly alongside the monad value, wires the CPU bus master to it, and needs no two-pass extraction.

This eliminates `harvestCPU` from the monad, removes the two-pass design entirely, and makes the address constants naturally available at the point the bus is assembled.

---

## Clash synthesis constraints

| Constraint | Reason |
|-----------|--------|
| Default `clash-inline-limit` (20) must not be exceeded | Higher limits cause exponential term growth. |
| `BusDef bus a` must be a pure product type | A State monad introduces non-repr function closures that Clash inlines once per bind × two passes. |
| `Synth dom` must be flat `env → st → (a, st)` | Nested transformers double the `>>=` inline count. |
| `SynthPureSt` must contain no `Signal` fields | Clash must project through it to reach the concrete address map. |
| `addrMapFromSpecs` must be `NOINLINE` | Prevents the specialiser from firing before the argument is concrete. |
