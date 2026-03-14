#!/usr/bin/env python3
# /// script
# requires-python = ">=3.14"
# ///
"""Generate baseline (unoptimized) program bundle and memory image for the Haskell simulator."""

import json
import os
import random
import struct
import sys
from pathlib import Path
from typing import Any

REPO_ROOT = Path(__file__).resolve().parents[1]
REFERENCE_DIR = REPO_ROOT / "ref" / "original_performance_takehome"
sys.path.insert(0, str(REFERENCE_DIR))

from perf_takehome import KernelBuilder  # type: ignore
from problem import Input, Tree, build_mem_image  # type: ignore

Instruction = dict[str, list[tuple[Any, ...]]]


def _render_tuple(items: list[str]) -> str:
    if not items:
        return "()"
    if len(items) == 1:
        return f"({items[0]},)"
    return "(" + ", ".join(items) + ")"


def _render_slot(slot: tuple[Any, ...]) -> str:
    rendered = [json.dumps(slot[0])]
    rendered.extend(str(int(value)) for value in slot[1:])
    return _render_tuple(rendered)


def _render_bundle(bundle: Instruction) -> str:
    fields: list[str] = []
    for engine in ("alu", "valu", "load", "store", "flow"):
        slots = bundle.get(engine, [])
        if slots:
            rendered_slots = "[" + ", ".join(_render_slot(s) for s in slots) + "]"
            fields.append(f"{json.dumps(engine)}: {rendered_slots}")
    return "{" + ", ".join(fields) + "}"


def render_program_payload(
    rounds: int,
    batch_size: int,
    program: list[Instruction],
) -> str:
    bundles = "[" + ", ".join(_render_bundle(b) for b in program) + "]"
    return (
        "{"
        + f'"rounds": {rounds}, '
        + f'"batch_size": {batch_size}, '
        + f'"program": {bundles}'
        + "}\n"
    )


def strip_debug(instrs: list[Instruction]) -> list[Instruction]:
    """Remove debug engine from bundles; drop bundles that become empty."""
    result = []
    for instr in instrs:
        cleaned = {k: v for k, v in instr.items() if k != "debug"}
        if cleaned:
            result.append(cleaned)
    return result


def write_mem_file(path: str, mem: list[int]) -> None:
    with open(path, "wb") as f:
        for w in mem:
            f.write(struct.pack("<I", w % (1 << 32)))


def main() -> None:
    forest_height = 10
    rounds = 16
    batch_size = 256
    seed = 123

    out_dir = sys.argv[1] if len(sys.argv) > 1 else "/tmp"
    if len(sys.argv) > 3:
        rounds = int(sys.argv[2])
        batch_size = int(sys.argv[3])

    os.makedirs(out_dir, exist_ok=True)

    random.seed(seed)
    forest = Tree.generate(forest_height)
    inp = Input.generate(forest, batch_size, rounds)
    mem = build_mem_image(forest, inp)

    kb = KernelBuilder()
    kb.build_kernel(forest.height, len(forest.values), len(inp.indices), rounds)

    program = strip_debug(kb.instrs)

    bundle_path = os.path.join(out_dir, "baseline.bundle")
    mem_path = os.path.join(out_dir, "baseline.bin")

    with open(bundle_path, "w") as f:
        f.write(render_program_payload(rounds, batch_size, program))

    write_mem_file(mem_path, mem)

    print(f"Program: {bundle_path} ({len(program)} bundles)")
    print(f"Memory:  {mem_path} ({len(mem)} words)")
    print(f"Config:  rounds={rounds}, batch_size={batch_size}, seed={seed}")
    print()
    print("Run with:")
    print(f"  cabal run simulator -- {bundle_path} -m {mem_path}")


if __name__ == "__main__":
    main()
