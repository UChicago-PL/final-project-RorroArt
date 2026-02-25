from __future__ import annotations

import ast
import json
import os
import re
import shlex
import struct
import subprocess
import sys
import textwrap
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import pytest
from hypothesis import HealthCheck, given, settings, strategies as st

REPO_ROOT = Path(__file__).resolve().parents[1]
REFERENCE_DIR = REPO_ROOT / "ref" / "original_performance_takehome"
if str(REFERENCE_DIR) not in sys.path:
    sys.path.insert(0, str(REFERENCE_DIR))

from problem import DebugInfo, Machine, SLOT_LIMITS  # type: ignore


Instruction = dict[str, list[tuple[Any, ...]]]
UINT32_MASK = (1 << 32) - 1
MEM_SIZE_WORDS = 32
ADDR_REGS = (0, 1, 2, 3)
ONE_REG = 4
VAL_REGS = tuple(range(5, 16))
ALL_SRC_REGS = ADDR_REGS + (ONE_REG,) + VAL_REGS
HASKELL_SIM_CMD_ENV = "HASKELL_SIM_CMD"


@dataclass(frozen=True)
class SimulationResult:
    mem: list[int]
    cycle: int | None


def _render_list(render_fn: Any, xs: list[Any]) -> str:
    return "[" + ",".join(render_fn(x) for x in xs) + "]"


def _render_tuple(items: list[str]) -> str:
    if not items:
        return "()"
    if len(items) == 1:
        return f"({items[0]},)"
    return "(" + ",".join(items) + ")"


def _render_slot(slot: tuple[Any, ...]) -> str:
    rendered = [json.dumps(slot[0])]
    rendered.extend(str(int(value)) for value in slot[1:])
    return _render_tuple(rendered)


def _render_bundle(bundle: Instruction) -> str:
    fields: list[str] = []
    for engine in ("alu", "valu", "load", "store", "flow"):
        slots = bundle.get(engine, [])
        if slots:
            fields.append(f"{json.dumps(engine)}:{_render_list(_render_slot, slots)}")
    return "{" + ",".join(fields) + "}"


def render_program_payload(
    rounds: int,
    batch_size: int,
    program: list[Instruction],
) -> str:
    return (
        "{"
        + f"{json.dumps('rounds')}:{rounds},"
        + f"{json.dumps('batch_size')}:{batch_size},"
        + f"{json.dumps('program')}:{_render_list(_render_bundle, program)}"
        + "}\n"
    )


def _normalize_slot(slot: Any) -> tuple[Any, ...]:
    if isinstance(slot, tuple):
        return slot
    if isinstance(slot, list):
        return tuple(slot)
    raise TypeError(f"Slot must be list/tuple, got {type(slot)!r}")


def parse_program_payload(payload_text: str) -> tuple[int, int, list[Instruction]]:
    raw = ast.literal_eval(payload_text)
    assert isinstance(raw, dict), f"Program payload must be dict, got {type(raw)!r}"
    assert "rounds" in raw and "batch_size" in raw and "program" in raw, (
        "Program payload must include rounds, batch_size, and program."
    )
    rounds = int(raw["rounds"])
    batch_size = int(raw["batch_size"])
    bundles_raw = raw["program"]
    assert isinstance(bundles_raw, list), "`program` must be a list of bundles."

    program: list[Instruction] = []
    for bundle in bundles_raw:
        assert isinstance(bundle, dict), f"Bundle must be a dict, got {type(bundle)!r}"
        normalized: Instruction = {}
        for engine, slots_raw in bundle.items():
            assert isinstance(engine, str), f"Engine key must be string, got {type(engine)!r}"
            assert isinstance(slots_raw, list), f"{engine} slots must be list, got {type(slots_raw)!r}"
            normalized[engine] = [_normalize_slot(slot) for slot in slots_raw]
        program.append(normalized)

    return rounds, batch_size, program


def write_mem_file(path: Path, mem: list[int]) -> None:
    data = b"".join(struct.pack("<I", int(value) & UINT32_MASK) for value in mem)
    path.write_bytes(data)


def read_mem_file(path: Path) -> list[int]:
    data = path.read_bytes()
    assert len(data) % 4 == 0, f"Memory file length must be multiple of 4, got {len(data)} bytes."
    return [word for (word,) in struct.iter_unpack("<I", data)]


def run_reference(mem: list[int], program: list[Instruction]) -> SimulationResult:
    machine = Machine(
        mem_dump=list(mem),
        program=program,
        debug_info=DebugInfo(scratch_map={}),
        n_cores=1,
    )
    machine.enable_pause = False
    machine.enable_debug = False
    machine.run()
    return SimulationResult(mem=machine.mem, cycle=machine.cycle)


def _parse_cycle(stdout: str) -> int | None:
    stripped = stdout.strip()
    if not stripped:
        return None
    try:
        raw = json.loads(stripped)
        if isinstance(raw, dict) and "cycle" in raw and isinstance(raw["cycle"], int):
            return raw["cycle"]
    except json.JSONDecodeError:
        pass

    if re.fullmatch(r"\d+", stripped):
        return int(stripped)

    match = re.search(r"(?i)\bcycles?\b\s*[:=]\s*(\d+)", stripped)
    if match:
        return int(match.group(1))
    return None


def run_candidate(command: list[str], program_path: Path, mem_path: Path) -> SimulationResult:
    full_cmd = command + [str(program_path), "-m", str(mem_path)]
    try:
        completed = subprocess.run(
            full_cmd,
            text=True,
            capture_output=True,
            check=False,
            timeout=20,
        )
    except subprocess.TimeoutExpired as exc:
        raise AssertionError(f"Simulator command timed out: {' '.join(full_cmd)}") from exc

    assert completed.returncode == 0, (
        f"Simulator command failed with code {completed.returncode}\n"
        f"Command: {' '.join(full_cmd)}\n"
        f"stdout:\n{completed.stdout}\n"
        f"stderr:\n{completed.stderr}"
    )
    return SimulationResult(mem=read_mem_file(mem_path), cycle=_parse_cycle(completed.stdout))


def assert_simulators_match(
    command: list[str],
    mem: list[int],
    program: list[Instruction],
    tmp_path: Path,
    *,
    rounds: int = 1,
    batch_size: int = 1,
) -> None:
    expected = run_reference(mem, program)

    program_path = tmp_path / "program.bundle"
    mem_path = tmp_path / "mem.bin"
    payload_text = render_program_payload(rounds, batch_size, program)
    program_path.write_text(payload_text, encoding="utf-8")
    write_mem_file(mem_path, mem)

    actual = run_candidate(command, program_path, mem_path)

    assert actual.mem == expected.mem, (
        "Memory mismatch between candidate and reference simulator.\n"
        f"Command: {' '.join(command)} <program.bundle> -m <mem.bin>\n"
        f"Payload:\n{payload_text}\n"
        f"Expected mem:\n{expected.mem}\n"
        f"Actual mem:\n{actual.mem}"
    )

    if actual.cycle is not None:
        assert actual.cycle == expected.cycle, (
            "Cycle mismatch between candidate and reference simulator.\n"
            f"Expected cycle: {expected.cycle}\n"
            f"Actual cycle: {actual.cycle}"
        )


def _write_fake_simulator(tmp_path: Path, *, broken: bool) -> list[str]:
    script_path = tmp_path / ("fake_broken_simulator.py" if broken else "fake_good_simulator.py")
    script = textwrap.dedent(
        f"""
        import argparse
        import ast
        import json
        import struct
        import sys
        from pathlib import Path

        REF_DIR = Path({str(REFERENCE_DIR)!r})
        if str(REF_DIR) not in sys.path:
            sys.path.insert(0, str(REF_DIR))

        from problem import DebugInfo, Machine

        BROKEN = {broken}
        UINT32_MASK = (1 << 32) - 1

        def decode_program(payload):
            program = []
            for bundle in payload["program"]:
                converted = {{}}
                for engine, slots in bundle.items():
                    converted[engine] = [tuple(slot) if isinstance(slot, list) else slot for slot in slots]
                program.append(converted)
            return program

        def read_mem(path):
            data = path.read_bytes()
            if len(data) % 4 != 0:
                raise ValueError("mem file length must be multiple of 4")
            return [word for (word,) in struct.iter_unpack("<I", data)]

        def write_mem(path, mem):
            data = b"".join(struct.pack("<I", int(v) & UINT32_MASK) for v in mem)
            path.write_bytes(data)

        def main():
            parser = argparse.ArgumentParser()
            parser.add_argument("program_path")
            parser.add_argument("-m", "--mem", required=True, dest="mem_path")
            args = parser.parse_args()

            payload = ast.literal_eval(Path(args.program_path).read_text(encoding="utf-8"))
            mem = read_mem(Path(args.mem_path))
            program = decode_program(payload)

            machine = Machine(
                mem_dump=mem,
                program=program,
                debug_info=DebugInfo(scratch_map={{}}),
                n_cores=1,
            )
            machine.enable_pause = False
            machine.enable_debug = False
            machine.run()

            if BROKEN and machine.mem:
                machine.mem[0] = (machine.mem[0] + 1) & UINT32_MASK

            write_mem(Path(args.mem_path), machine.mem)
            sys.stdout.write(json.dumps({{"cycle": machine.cycle}}))

        if __name__ == "__main__":
            main()
        """
    ).strip()
    script_path.write_text(script + "\n", encoding="utf-8")
    return [sys.executable, str(script_path)]


def _init_bundles(address_values: list[int]) -> list[Instruction]:
    slots = [("const", reg, value) for reg, value in zip(ADDR_REGS, address_values, strict=True)]
    slots.append(("const", ONE_REG, 1))
    chunk_size = SLOT_LIMITS["load"]
    return [{"load": slots[i : i + chunk_size]} for i in range(0, len(slots), chunk_size)]


U32S = st.integers(min_value=0, max_value=UINT32_MASK)
ADDR_VALUES = st.integers(min_value=0, max_value=MEM_SIZE_WORDS - 1)
DEST_REG = st.sampled_from(VAL_REGS)
SRC_REG = st.sampled_from(ALL_SRC_REGS)
ADDR_REG = st.sampled_from(ADDR_REGS)
SHIFT_SRC_REG = st.sampled_from(ADDR_REGS + (ONE_REG,))
BASE_ALU_OP = st.sampled_from(["+", "-", "*", "^", "&", "|", "<", "=="])
SHIFT_ALU_OP = st.sampled_from(["<<", ">>"])
DIV_ALU_OP = st.sampled_from(["//", "cdiv", "%"])
ALU_SLOT = st.one_of(
    st.tuples(BASE_ALU_OP, DEST_REG, SRC_REG, SRC_REG),
    st.tuples(SHIFT_ALU_OP, DEST_REG, SRC_REG, SHIFT_SRC_REG),
    st.tuples(DIV_ALU_OP, DEST_REG, SRC_REG, st.just(ONE_REG)),
)
LOAD_SLOT = st.one_of(
    st.tuples(st.just("const"), DEST_REG, U32S),
    st.tuples(st.just("load"), DEST_REG, ADDR_REG),
)
STORE_SLOT = st.tuples(st.just("store"), ADDR_REG, SRC_REG)
FLOW_SLOT = st.one_of(
    st.tuples(st.just("select"), DEST_REG, SRC_REG, SRC_REG, SRC_REG),
    st.tuples(st.just("pause")),
)
RAW_BUNDLE = st.fixed_dictionaries(
    {
        "alu": st.lists(ALU_SLOT, max_size=min(4, SLOT_LIMITS["alu"])),
        "load": st.lists(LOAD_SLOT, max_size=SLOT_LIMITS["load"]),
        "store": st.lists(STORE_SLOT, max_size=SLOT_LIMITS["store"]),
        "flow": st.lists(FLOW_SLOT, max_size=SLOT_LIMITS["flow"]),
    }
)
BUNDLE = RAW_BUNDLE.map(
    lambda bundle: {engine: slots for engine, slots in bundle.items() if slots}
).filter(bool)


@st.composite
def simulator_case(draw: st.DrawFn) -> tuple[list[int], list[Instruction]]:
    mem = draw(st.lists(U32S, min_size=MEM_SIZE_WORDS, max_size=MEM_SIZE_WORDS))
    addrs = draw(st.lists(ADDR_VALUES, min_size=len(ADDR_REGS), max_size=len(ADDR_REGS)))
    random_bundles = draw(st.lists(BUNDLE, min_size=1, max_size=24))
    return mem, _init_bundles(addrs) + random_bundles


def test_program_payload_round_trip() -> None:
    program: list[Instruction] = [
        {"load": [("const", 5, 11), ("const", 6, 7)]},
        {"alu": [("+", 7, 5, 6)], "store": [("store", 0, 7)]},
        {},
        {"flow": [("pause",)]},
    ]
    payload = render_program_payload(rounds=3, batch_size=8, program=program)
    rounds, batch_size, parsed = parse_program_payload(payload)
    assert rounds == 3
    assert batch_size == 8
    assert parsed == program


def test_mem_file_round_trip(tmp_path: Path) -> None:
    mem = [0, 1, UINT32_MASK, 42, 7]
    mem_path = tmp_path / "mem.bin"
    write_mem_file(mem_path, mem)
    assert read_mem_file(mem_path) == mem


def test_fake_correct_simulator_matches_reference(tmp_path: Path) -> None:
    cmd = _write_fake_simulator(tmp_path, broken=False)
    mem = [0] * MEM_SIZE_WORDS
    program = _init_bundles([1, 2, 3, 4]) + [{"load": [("const", 5, 123)]}]
    assert_simulators_match(cmd, mem, program, tmp_path)


def test_fake_broken_simulator_is_detected(tmp_path: Path) -> None:
    cmd = _write_fake_simulator(tmp_path, broken=True)
    mem = [0] * MEM_SIZE_WORDS
    program = _init_bundles([1, 2, 3, 4]) + [{"load": [("const", 5, 123)]}]

    with pytest.raises(AssertionError, match="Memory mismatch"):
        assert_simulators_match(cmd, mem, program, tmp_path)


@settings(
    max_examples=80,
    deadline=None,
    suppress_health_check=[HealthCheck.function_scoped_fixture],
)
@given(case=simulator_case())
def test_hypothesis_cases_match_reference_with_fake_good_simulator(
    tmp_path: Path,
    case: tuple[list[int], list[Instruction]],
) -> None:
    cmd = _write_fake_simulator(tmp_path, broken=False)
    mem, program = case
    assert_simulators_match(cmd, mem, program, tmp_path)


@pytest.mark.skipif(
    not os.getenv(HASKELL_SIM_CMD_ENV),
    reason=(
        "Set HASKELL_SIM_CMD to your simulator base command "
        "(example: `HASKELL_SIM_CMD='cabal run simulator --'`). "
        "The harness appends `<program.bundle> -m <mem.bin>`."
    ),
)
@settings(max_examples=120, deadline=None)
@given(case=simulator_case())
def test_haskell_simulator_matches_reference(
    tmp_path: Path,
    case: tuple[list[int], list[Instruction]],
) -> None:
    cmd = shlex.split(os.environ[HASKELL_SIM_CMD_ENV])
    mem, program = case
    assert_simulators_match(cmd, mem, program, tmp_path)
