## PBT Simulator Harness

This folder contains property-based tests that compare a simulator command
against the Python reference simulator in
`ref/original_performance_takehome/problem.py`.

### Setup

```bash
cd pbt
uv sync
uv run pytest -q
```

### Simulator Contract (File-based)

The harness now uses a compiler-aligned file interface:

```bash
simulator <program.bundle> -m <mem.bin>
```

- `<program.bundle>`: text payload in the same shape emitted by
  `app/compiler/PythonInterop.renderProgramPayload` (`rounds`, `batch_size`,
  `program` bundles with tuple slots; compiler currently emits `alu/valu/load/store/flow`
  sections and omits debug slots.
- `<mem.bin>`: raw bytes, little-endian `uint32` words.
- Simulator should update `<mem.bin>` in place to final memory contents.
- Simulator stdout may optionally include cycle information (JSON with
  `"cycle"`, a plain integer, or `cycles: <n>` style text).

### Program Bundle Format

Example payload:

```text
{"rounds":1,"batch_size":2,"program":[{"load":[("const",5,123)]},{"alu":[("+",6,5,5)],"store":[("store",0,6)]}]}
```

### Running Against The Haskell Simulator

By default, tests validate the harness using temporary fake simulators
(one correct, one intentionally broken), and auto-clean them.

To run against your real simulator:

```bash
cd pbt
HASKELL_SIM_CMD="cabal run simulator --" uv run pytest -q
```

The harness appends `<program.bundle> -m <mem.bin>`.
The `test_haskell_simulator_matches_reference` property is skipped unless
`HASKELL_SIM_CMD` is set.
