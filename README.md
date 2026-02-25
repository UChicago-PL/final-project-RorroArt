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

Optionally, run the Python simulator against the exported payload:
```bash
python3 /Users/aryavohra/fp/vliw_monad/tools/run_hs_program.py --payload /tmp/fp_final_project_baseline.pydata
```
