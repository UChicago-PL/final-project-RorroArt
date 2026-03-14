#!/usr/bin/env python3
# /// script
# requires-python = ">=3.14"
# ///
"""Streaming reference simulator: reads JSON lines from stdin, writes traces to stdout."""

import json
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT / "ref" / "original_performance_takehome"))

from problem import DebugInfo, Machine  # type: ignore


def run_one(program_json, mem_json):
    program = [
        {engine: [tuple(s) for s in slots] for engine, slots in bundle.items()}
        for bundle in program_json
    ]
    machine = Machine(
        mem_dump=list(mem_json),
        program=program,
        debug_info=DebugInfo(scratch_map={}),
    )
    machine.enable_pause = False
    machine.enable_debug = False
    machine.run()
    return [
        {
            "cycle": ct["cycle"],
            "pc": ct["pc"],
            "scratch_w": {str(k): v for k, v in ct["scratch_w"].items()},
            "mem_w": {str(k): v for k, v in ct["mem_w"].items()},
            "next_pc": ct["next_pc"],
            "run_state": ct["run_state"],
        }
        for ct in machine.cycle_trace
    ]


def main():
    sys.stdout.write("ready\n")
    sys.stdout.flush()

    for line in sys.stdin:
        line = line.strip()
        if not line or line == "quit":
            break
        try:
            req = json.loads(line)
            trace = run_one(req["program"], req["mem"])
            sys.stdout.write(json.dumps({"ok": trace}) + "\n")
        except Exception as e:
            sys.stdout.write(json.dumps({"error": str(e)}) + "\n")
        sys.stdout.flush()


if __name__ == "__main__":
    main()
