# Usage
## Build both modules
```bash
cabal build compiler simulator
```

## Check FinalIR lowering path
```bash
cabal run compiler -- check-ir-lowering
```

## Export a baseline payload
```bash
cabal run compiler -- export-baseline /tmp/fp_final_project_baseline.pydata
```

## Run with the Haskell simulator
```bash
cabal run simulator -- <program.bundle> -m <memory.bin>
```
Add `--trace` for cycle-by-cycle output.

## Run property-based tests
```bash
cabal test pbt
```
This runs 1000 random programs against the Python reference simulator.
