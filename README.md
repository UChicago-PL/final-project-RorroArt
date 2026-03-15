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

## Compile and run end-to-end
Generate a memory image, compile the optimized program, and run it:
```bash
uv run scripts/gen_baseline.py /tmp/out
cabal run compiler -- /tmp/out/optimized.bundle
cabal run simulator -- /tmp/out/optimized.bundle -m /tmp/out/baseline.bin
```
Add `--trace` for cycle-by-cycle output.

## Run property-based tests
```bash
cabal test pbt
```
This runs 1000 random programs against the Python reference simulator.
