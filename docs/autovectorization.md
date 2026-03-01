# Auto-Vectorization in the FinalIR Compiler

This document walks through the FinalIR intermediate representation and the
auto-vectorization pipeline implemented in `BatchVectorizeFinalIR`. It covers
the IR structure, each transformation pass, and what kinds of programs can and
cannot be vectorized.

## Part 1: The IR

FinalIR is a structured, typed, SSA intermediate representation. Programs are
built out of four layers: **values**, **statements**, **regions**, and
**kernels**.

### Types

Every value has a type:

```
I32          -- 32-bit integer (also used for booleans: 0 = false)
Ptr          -- memory address
Vec w ty     -- SIMD vector of `w` lanes, each of type `ty` (e.g. Vec 8 I32)
Mask w       -- lane mask for vector selects/compares
Mem space    -- memory effect token (not a real value -- see below)
```

The `Mem` type deserves special attention. It's a *token* that threads memory
effects through the program, enforcing ordering. Every load consumes a `Mem`
and produces a new one; every store does the same. This makes the data-flow
graph capture memory ordering without needing explicit sequence points. A
program that loads A then stores B looks like:

```
let (val, mem1) = load mem0 addr_a     -- mem0 -> mem1
let mem2        = store mem1 addr_b x  -- mem1 -> mem2
```

Reordering these would require rewiring the `mem` chain, which makes illegal
reorderings visible as type errors.

### Addresses

Memory operations take an `Addr`, which is a base pointer plus an index:

```
Addr base index
```

The index can be either:

- `IndexVal id` -- an arbitrary SSA value used as offset (e.g. an indirection
  through a lookup table)
- `IndexAff ix` -- an affine expression built from constants, SSA variables,
  lane indices, addition, and scaling

The affine form is what lets the vectorizer distinguish contiguous access
(`base[i]`) from irregular access (`base[table[i]]`).

### Statements

```
Let [(id, ty)] rhs        -- bind the result of an operation
Eff effect                 -- side effect (pause, halt, debug, trace)
If cond thenRegion elseRegion outs   -- structured conditional
For exec lb ub step inits body outs  -- structured loop
```

Every compound statement (`If`, `For`) contains **regions** -- self-contained
blocks with parameters, a body, and yield values. A `For` loop's region
receives an induction variable plus carry parameters, and yields updated carry
values each iteration. This is how loop-carried state is modeled in SSA without
phi nodes.

A simple "add one to each element" loop:

```
-- kernel params: mem0 : Mem
let inpPtr : Ptr  = load meta slot 4
let outPtr : Ptr  = load meta slot 5
let ub     : I32  = const 256
let one    : I32  = const 1

for i in [0..ub) step 1, carrying [mem0]:
  region params: (i : I32, memIn : Mem)
    let val, mem1  = load memIn (Addr inpPtr (IndexVal i))
    let val'       = add val one
    let mem2       = store mem1 (Addr outPtr (IndexVal i)) val'
  yield [mem2]
  outs: [(memOut : Mem)]
```

### RHS operations

Value-producing operations include:

- `RConst w` -- 32-bit constant
- `RBin op a b` -- binary ALU (Add, Mul, Xor, Shl, Lt, Eq_, Mod, ...)
- `RSelect c a b` -- ternary select (c ? a : b)
- `RBroadcast w src` -- splat scalar to all vector lanes
- `RReduce op src` -- horizontal reduction of a vector to scalar (e.g. sum all
  8 lanes)
- `RLoad mem addr` / `RStore mem addr val` -- memory operations
- `RCoreId` -- hardware core identifier
- `RMultiplyAdd a b c` -- fused multiply-add

---

## Part 2: Address Canonicalization

This is a pre-pass that normalizes how addresses are expressed, making patterns
visible to the vectorizer.

### The problem

The kernel builder constructs addresses in two steps:

```
let ptr : Ptr = add basePtr i      -- compute address as Ptr arithmetic
let val, mem1 = load mem (Addr ptr (IxConst 0))   -- load from that address
```

The load's address says "base = `ptr`, offset = 0." The fact that `ptr` itself
is `basePtr + i` is hidden -- the vectorizer would have to chase through
definitions to discover the index depends on the loop induction variable.

### The transformation

Canonicalization rewrites this to:

```
let val, mem1 = load mem (Addr basePtr (IndexVal i))
```

Now the address directly expresses "base = `basePtr`, index = `i`", and the
vectorizer can inspect `i`'s relationship to the loop IV without any def-use
chasing.

Concretely, the pass:

1. Scans for `Let [(out, Ptr)] (RBin Add base idx)` definitions
2. Rewrites any load/store whose address is `Addr out (IxConst 0)` to
   `Addr base (IndexVal idx)`
3. Removes the original `Ptr Add` if it's now unused (dead code elimination)

### Limitations

Only matches the exact pattern `Addr base (IxConst 0)` -- a non-zero constant
offset like `Addr base (IxConst 3)` won't be rewritten. This is fine because
the kernel builders always produce the zero-offset form, but a more general
canonicalization would handle `Addr base (IxConst k)` by folding `k` into the
rewritten index.

---

## Part 3: If-Conversion

This pre-pass transforms structured `If` statements into `Select` operations,
enabling vectorization of loops containing branches.

### The problem

The vectorizer only handles flat loop bodies (sequences of `Let` and `Eff`). A
loop with an `If` inside would be rejected:

```
for i in [0..24) step 1, carrying [mem0]:
  let x, mem1 = load memIn (Addr inpPtr (IndexVal i))
  let cond    = lt x zero
  if cond:
    then: let a = add x one;    yield [a]
    else: let b = xor x salt;   yield [b]
    outs: [(y : I32)]
  let mem2 = store mem1 (Addr outPtr (IndexVal i)) y
  yield [mem2]
```

### The transformation

When both branches are *pure* (no loads or stores, just arithmetic), the `If`
is converted to selects:

```
for i in [0..24) step 1, carrying [mem0]:
  let x, mem1 = load memIn (Addr inpPtr (IndexVal i))
  let cond    = lt x zero
  let a       = add x one        -- then branch, always computed
  let b       = xor x salt       -- else branch, always computed
  let y       = select cond a b  -- mux the results
  let mem2    = store mem1 (Addr outPtr (IndexVal i)) y
  yield [mem2]
```

Now the loop body is flat and vectorizable. Both branches execute
speculatively, and the `select` picks the right result per-lane once
vectorized.

### What can't be if-converted

If either branch contains a load or store, conversion is rejected --
speculative memory operations could fault or produce observable side effects.
The `If` remains structured, and the loop is left scalar.

---

## Part 4: Vectorization (BatchVectorizeFinalIR)

This is the main transformation. It converts scalar loops into SIMD loops that
process 8 elements per iteration.

### Step 1: Legality analysis

A loop can be vectorized only if **all** of these hold:

1. **Scalar execution mode** (`ExecScalar`) with **step 1**
2. **Compile-time-constant upper bound** (the lowerer unrolls loops, so it
   needs to know the trip count)
3. **Flat body** -- only `Let` and `Eff` statements (after if-conversion)
4. **Legal carries** -- every loop-carried variable is either a `Mem` token
   (passed through) or an *associative reduction* (see below)
5. **No unsupported effects** -- no `EPause`, `ETraceWrite`, or `EHalt` in the
   body
6. **Unit-stride or gather/scatter addresses** -- no affine stride other than 1

### Step 2: Carry analysis

The vectorizer inspects each carry variable to classify it:

- **Mem tokens**: pass through unchanged (memory ordering is orthogonal to
  vectorization)
- **Associative reductions**: a carry of the form `acc' = op acc x` where `op`
  is `Add`, `Mul`, `Xor`, `And`, or `Or`. These can be vectorized by
  accumulating per-lane then reducing horizontally at the end.

Example of a vectorizable reduction:

```
for i in [0..32) step 1, carrying [mem0, acc=0]:
  let x, mem1 = load memIn (Addr inpPtr (IndexVal i))
  let acc'    = add acc x     -- associative carry: acc' = acc + x
  yield [mem1, acc']
```

Example of a **non-vectorizable** carry (prefix sum -- the yield is also used
inside the body):

```
for i in [0..32) step 1, carrying [mem0, acc=0]:
  let x, mem1 = load memIn (Addr inpPtr (IndexVal i))
  let acc'    = add acc x
  let mem2    = store mem1 (Addr outPtr (IndexVal i)) acc'  -- acc' used in body
  yield [mem2, acc']
```

Here each iteration's store depends on the *running* sum, so iteration `i+1`
can't start until iteration `i` completes. The vectorizer detects this (the
reduction output `acc'` appears in the body's use set) and leaves the loop
scalar.

### Step 3: Strip-mining

A loop `for i in [0..256) step 1` becomes:

```
for i in [0..256) step 8  [ExecSimd, vectorized body]
```

If the trip count isn't a multiple of 8 (e.g., 18):

```
let vecUb = const 16
for i in [0..vecUb) step 8  [ExecSimd, vectorized body]
for i in [16..ub) step 1    [ExecScalar, original body]
```

The tail loop handles the remainder with the original scalar code.

### Step 4: Mode classification

Every value in the loop body is classified into one of three *modes*:

| Mode | Meaning | Example |
|---|---|---|
| **Uniform** | Same value across all 8 lanes | A constant, or a value defined outside the loop |
| **LaneBase** | Varies as a linear function of the lane index | The induction variable `i`, or `i + constant` |
| **Vec** | Arbitrary per-lane values | Data loaded from memory, or any operation on a Vec value |

Mode propagation follows simple rules:

- Constants and outer-scope values -> Uniform
- The induction variable -> LaneBase
- `Uniform op Uniform` -> Uniform
- `Uniform op LaneBase` or `LaneBase op Uniform` -> LaneBase
- Anything involving `Vec` -> Vec

When a `Vec` operation needs a `Uniform` or `LaneBase` input, the vectorizer
inserts a **broadcast** (`RBroadcast 8 scalar`), which is cached to avoid
duplicates.

### Step 5: Statement transformation

Each scalar statement is rewritten to its vector equivalent:

**Binary ops**: If either operand is `Vec`, both are promoted to vectors
(broadcast if needed), and the output becomes `Vec 8 I32`:

```
-- Scalar:   let y : I32 = xor x salt
-- Vector:   let x_vec  : Vec 8 I32 = ... (already Vec from a load)
--           let salt_v : Vec 8 I32 = broadcast 8 salt
--           let y_vec  : Vec 8 I32 = xor x_vec salt_v
```

**Loads**: The address index determines the access pattern:

- *LaneBase index* (contiguous) -> `VLoad` (single wide load of 8 consecutive
  elements):

  ```
  -- Scalar:  load mem (Addr inpPtr (IndexVal i))
  -- Vector:  load mem (Addr inpPtr (IndexVal i))  with type Vec 8 I32
  --          -> lowered to VLoad (one instruction)
  ```

- *Vec index* (irregular) -> **gather** (8 individual loads):

  ```
  -- Scalar:  load mem (Addr dataPtr (IndexVal idx))     where idx varies per element
  -- Vector:  load mem (Addr dataPtr (IndexVal idx_vec))  with type Vec 8 I32
  --          -> lowered to 8 separate Load instructions (gather)
  ```

**Stores**: Same distinction -- contiguous `VStore` vs. **scatter** (8
individual stores). The vectorizer also ensures the value operand is broadcast
to a vector if the address requires a vector store.

**Selects**: All three operands are promoted to vectors if any one is `Vec`:

```
-- Scalar:  let y = select cond x zero
-- Vector:  let y_vec = vselect cond_vec x_vec zero_vec
```

**Reductions**: When a binary op is identified as a reduction carry, the
vectorizer emits:

1. A vector accumulation (the per-lane partial results)
2. An `RReduce` to horizontally fold the vector to a scalar
3. A final `RBin` to combine with the incoming scalar carry

```
-- Scalar:  acc' = add acc x
-- Vector:  let x_vec     : Vec 8 I32 = vload ...
--          let partial   : I32       = reduce add x_vec    -- sum 8 lanes
--          let acc'      : I32       = add acc partial     -- fold into carry
```

**Debug compares**: If the compared value became a vector, a scalar
`EDebugCompare` is expanded to `EDebugCompareV` with 8 per-lane keys. The
debug policy (a configurable record of functions) handles key substitution,
keeping the vectorizer core agnostic to the debug key type.

### Step 6: Lane form analysis (for legality)

Before vectorizing, the pass analyzes every address to determine its *lane
form* -- how the index relates to the induction variable:

| Lane form | Meaning | Vectorizable? |
|---|---|---|
| `LFUniform` | Independent of IV | Yes (broadcast) |
| `LFLane 1` | `base + i` (stride 1) | Yes (contiguous VLoad/VStore) |
| `LFLane k` where k != 1 | `base + k*i` (stride k) | **No** -- rejected |
| `LFUnknown` | Can't determine statically | Yes (treated as gather/scatter) |

The analysis traces through definitions recursively. For example,
`let scaled = mul i two; let addr = add ptr scaled` is analyzed as: `scaled` =
`LFLane 2` (IV times constant 2), so the address has stride 2 -- rejected.
This prevents emitting a contiguous VLoad for data that's actually interleaved.

---

## Summary: What Can and Can't Be Vectorized

### Can vectorize

- **Contiguous array maps**: `out[i] = f(in[i])` for arbitrary `f` built from
  arithmetic, selects, and constants
- **Gather patterns**: `out[i] = data[index[i]]` -- loads through indirection
  arrays
- **Scatter patterns**: `data[index[i]] = in[i]` -- stores through indirection
  arrays
- **Mixed gather + contiguous** in the same loop
- **Associative reductions**: `acc = acc + in[i]` (or `*`, `^`, `&`, `|`)
- **Pure branches**: `if cond then a else b` (if-converted to selects)
- **The full baseline hash kernel** (loads, 6-stage hash, tree index
  computation, stores -- with scatter/gather for the forest lookup)

### Cannot vectorize (stays scalar)

- **Strided access**: `in[2*i]` -- stride != 1 is rejected rather than
  emitting a gather, since it might indicate the programmer intended something
  the vectorizer shouldn't silently change
- **Prefix sums / scan patterns**: `acc = acc + in[i]; out[i] = acc` -- the
  carry result is used inside the body, creating a true loop-carried dependence
- **TraceWrite in loop body**: side effects that can't be meaningfully
  vectorized
- **EPause / EHalt in loop body**: control effects
- **Branches with memory operations**: `if cond then load(...) else store(...)`
  -- can't be if-converted because speculative memory ops are unsafe
- **Trip count < 8**: not enough iterations to fill a single vector
- **Non-constant trip count**: the lowerer requires compile-time bounds for
  unrolling
- **Nested loops in the body**: inner `For` blocks are rejected (only flat
  Let/Eff bodies)
