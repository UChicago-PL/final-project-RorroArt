#!/usr/bin/env python3
# /// script
# requires-python = ">=3.14"
# ///
"""Run the Python reference simulator and output cycle trace as JSON."""

import json
import struct
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
REFERENCE_DIR = REPO_ROOT / "ref" / "original_performance_takehome"
sys.path.insert(0, str(REFERENCE_DIR))

from problem import DebugInfo, Machine  # type: ignore


def read_mem_file(path: str) -> list[int]:
    with open(path, "rb") as f:
        data = f.read()
    n = len(data) // 4
    return [struct.unpack_from("<I", data, i * 4)[0] for i in range(n)]


def load_program(path: str) -> list[dict]:
    with open(path) as f:
        bundles = json.load(f)
    result = []
    for bundle in bundles:
        converted = {}
        for engine, slots in bundle.items():
            converted[engine] = [tuple(s) for s in slots]
        result.append(converted)
    return result


def main():
    if len(sys.argv) != 3:
        print("Usage: run_ref.py <program.json> <mem.bin>", file=sys.stderr)
        sys.exit(1)

    program = load_program(sys.argv[1])
    mem = read_mem_file(sys.argv[2])

    machine = Machine(
        mem_dump=mem,
        program=program,
        debug_info=DebugInfo(scratch_map={}),
    )
    machine.enable_pause = False
    machine.enable_debug = False
    machine.run()

    trace = []
    for ct in machine.cycle_trace:
        trace.append(
            {
                "cycle": ct["cycle"],
                "pc": ct["pc"],
                "scratch_w": {str(k): v for k, v in ct["scratch_w"].items()},
                "mem_w": {str(k): v for k, v in ct["mem_w"].items()},
                "next_pc": ct["next_pc"],
                "run_state": ct["run_state"],
            }
        )

    json.dump(trace, sys.stdout)
    print()


if __name__ == "__main__":
    main()
