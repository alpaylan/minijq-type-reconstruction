# Type Reconstruction Prototype (`minijq`)

> Note!
> This repo is mostly a research prototype I've whipped up with Codex, it's possible there are some
> some empty corners.

Prototype for automatic input type reconstruction (inference) for a dynamically typed jq-like language using random testing and type subtraction.

## Core Idea

1. Start with a broad candidate input type (initially `Any`).
2. Infer a static input/output scheme for a filter.
3. Randomly generate inputs from the current candidate type.
4. Run the filter.
5. If a dynamic type error occurs, probe more samples from that failing region:
   - subtract it only when all probes fail
   - otherwise keep it and mark it as `Subset<T>`
6. Repeat until stable.

## Current Progress

Implemented:

- A `minijq` AST, parser, evaluator, type system, random generator, and reconstruction loop.
- Incremental reconstruction cache keyed by program name + source, persisted in `.mjqi` format:
  - `.minijq.typecache.mjqi` stores the reconstructed valid input domain, output type, subset markers, and convergence status.
- Structural JSON-like types with:
  - `Any`, `Never`, primitives.
  - scalar literal refinement: `Bool<true/false>`, `Number<...>`, `String<...>`.
  - `Array<T>`, `NonEmptyArray<T>`, tuples.
  - object shapes with required fields plus row tails.
  - unions and generics.
- Type operations:
  - normalization
  - subtyping
  - intersection
  - subtraction
- Type/scheme inference for expressions and builtins.
- Runtime type checking with detailed `RuntimeTypeError`.
- Subset markers for partially valid domains (`Subset<T>`), so reconstruction avoids over-constraining when only part of a type is invalid (e.g. `tonumber(.)` yields `Number | Subset<String>`).

Language features currently supported:

- navigation: `.field`, `.[idx]`, dynamic lookup `.[expr]`
- postfix optional error suppression: `expr?`
- `try expr catch expr`
- explicit failures: `error` and `error("msg")`
- literals: numbers (including floats), strings, booleans, null, arrays, objects
- operators: `+ - * /`, comparisons, `and`, `or`, `//`, unary `not` and unary `-`
- conditionals: `if ... then ... else ... end`
- piping: `lhs | rhs`

Builtins currently supported:

- `length`, `first`, `last`, `keys`, `type`
- `map`, `select`
- `has`, `contains`, `startswith`, `endswith`
- `reverse`, `sort`
- `error`, `tostring`, `tonumber`, `abs`, `floor`, `ceil`
- `add`, `min`, `max`, `split`, `join`

Validation status:

- `cargo test`: all tests passing (including jq compatibility checks).
- `cargo run`: all bundled examples converge.
- jq compatibility includes ported cases inspired by `tjq/tests/short/test1` and `tjq/tests/short/test4`.

## jq Compatibility Guide

Status legend:
- `Supported`: implemented and covered by tests/examples.
- `Partial`: implemented but semantics differ from jq in notable cases.
- `Missing`: not implemented yet.

| Area                                                                                                      | Status                             | Notes                                                                                                                                                                 |
| --------------------------------------------------------------------------------------------------------- | ---------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Core expressions (`.`, literals, `                                                                        | `, `if ... then ... else ... end`) | `Supported`                                                                                                                                                           | Includes your `if . == true or . == false then 1 else error end` pattern. |
| Error handling (`error`, `error("msg")`, `try ... catch ...`)                                             | `Supported`                        | Explicit errors are catchable and tested against jq.                                                                                                                  |
| Optional postfix (`expr?`)                                                                                | `Supported`                        | Suppresses runtime errors into `null`.                                                                                                                                |
| Field/index/lookup (`.k`, `.[n]`, `.[expr]`)                                                              | `Partial`                          | Non-negative indexing and dynamic lookup are supported; negative indices and slices are not.                                                                          |
| Comparisons and boolean ops (`==`, `!=`, `<`, `<=`, `>`, `>=`, `and`, `or`, `//`)                         | `Partial`                          | Mostly aligned; still needs broader parity validation on edge cases.                                                                                                  |
| Binary arithmetic operators (`+`, `-`, `*`, `/`)                                                          | `Partial`                          | Currently numeric-only for binary operators; jq overloads (string concat/repeat, object merge, array diff/concat, string split division) are not all implemented yet. |
| Builtins currently listed in README                                                                       | `Supported`                        | `length`, `map`, `select`, `contains`, `split`, `join`, `tonumber`, etc.                                                                                              |
| Streaming model (multi-output filters)                                                                    | `Missing`                          | Current evaluator returns a single JSON value, while jq is stream-based.                                                                                              |
| Iterator syntax (`.[]`)                                                                                   | `Missing`                          | Not parsed/evaluated in jq stream semantics.                                                                                                                          |
| Slice syntax (`.[a:b]`, `.[a:]`, `.[:b]`)                                                                 | `Missing`                          | Not parsed/evaluated.                                                                                                                                                 |
| Function definitions (`def f: ...;`) and calls                                                            | `Missing`                          | No `def`, `;`, or user-defined function environment yet.                                                                                                              |
| Variable bindings (`as $x`, `$x`)                                                                         | `Missing`                          | No binder/variable syntax support yet.                                                                                                                                |
| Structural control combinators (`reduce`, `foreach`, `until`)                                             | `Missing`                          | Not implemented in parser/evaluator/type inference.                                                                                                                   |
| Broader jq builtin surface (`range`, `del`, `to_entries`, `map_values`, `group_by`, regex builtins, etc.) | `Missing`                          | Only a focused subset is currently implemented.                                                                                                                       |
| Modules/imports                                                                                           | `Missing`                          | No module system.                                                                                                                                                     |

### Recommended parity order

1. Finish overloaded semantics for binary `+ - * /` (highest payoff for correctness).
2. Add stream core: `.[]`, then stream-aware `map`/`select` behavior.
3. Add slice indexing and negative indexing.
4. Add `def`, `as $x`, and `reduce` (unblocks many `tjq` cases).
5. Expand builtin set incrementally with jq-compat tests.

## Run

```bash
cargo test
cargo run
```

The first `cargo run` reconstructs valid input domains and populates `.minijq.typecache.mjqi`. Later runs reuse cached reconstruction results when sources are unchanged, and derive runtime-informed schemes from that cached domain.

Control cache behavior:

```bash
# custom cache location
cargo run -- --type-cache ./cache/minijq-types.mjqi

# force re-inference and overwrite cache entries
cargo run -- --refresh-type-cache

# disable cache for a run
cargo run -- --no-type-cache
```

Analyze a definitions file and emit a sibling interface file:

```bash
cargo run -- --analyze-file defs.mjq
# writes defs.mjqi
```

`defs.mjqi` stores runtime-informed schemes from reconstructed valid input domains for each analyzable `def`, and appends comments for unsupported definitions.

Validate a concrete JSON input against a filter using reconstructed input domains:

```bash
cargo run -- --validate-filter '.metrics.a + .metrics.b' \
  --validate-input '{"metrics":{"a":"oops","b":2}}'
```

You can also pass JSON via file:

```bash
cargo run -- --validate-filter 'map(.id + 1)' \
  --validate-input-file ./bad-input.json
```

The validator runs reconstruction for the target filter, executes the filter, and when runtime type errors occur prints:

- reconstructed input domain
- actual input type
- whether the input is accepted by reconstructed domain
- path-aware issues (e.g. `$["metrics"]["a"]: expected Number, got String<"oops">`)

### Validator Scenarios

Examples currently covered by tests:

1. Filter: `.metrics.a + .metrics.b`
   Bad input: `{"metrics":{"a":"oops","b":2}}`
   Diagnosis: `$["metrics"]["a"]` must be `Number`.

2. Filter: `map(.id + 1)`
   Bad input: `[{"id":1},{"id":"x"}]`
   Diagnosis: `$[1]["id"]` must be `Number`.

3. Filter: `.[0]`
   Bad input: `[]`
   Diagnosis: root input must be a non-empty array.

### jq Runtime Wrapper (`jqv`)

The repository now includes a `jqv` binary (`src/bin/jqv.rs`) that forwards arguments directly to your system `jq` and only intervenes when jq exits with runtime error code `5`.

On a runtime error, `jqv`:

- infers/reuses reconstructed input domains for the invoked filter
- replays the input through minijq analysis
- prints a validator report with path-aware diagnostics

Build and run locally:

```bash
cargo build --release --bin jqv
./target/release/jqv '.a + 1' input.json
```

You can override the jq executable path with `JQ_BIN`:

```bash
JQ_BIN=/usr/local/bin/jq ./target/release/jqv '.foo' input.json
```

### CI and Release

- `.github/workflows/ci.yml`: runs formatting checks and tests on PRs/pushes.
- `.github/workflows/release.yml`: on tag pushes like `v0.1.0`, builds `jqv` for Linux/macOS/Windows and publishes archives plus `SHA256SUMS.txt` to GitHub Releases.

## Curated Examples

For a ready-to-browse set of filters, inputs, jq outputs, and validator diagnostics, see:

- `examples/README.md`
- `examples/results/perf_summary.md` for jq vs jqv overhead numbers on the curated cases.
- includes a `large_mid_error` case with 50k records (50002-line JSON input) and a mid-array failure at index `25000`.

To regenerate all example result snapshots:

```bash
bash examples/generate_results.sh
```

Run jq compatibility tests only:

```bash
cargo test --test jq_compat
```

`jq_compat` tests expect `jq >= 1.7.1` (CI pins `jq-1.7.1`).

Trace one program's reconstruction loop:

```bash
cargo run -- --program strict_tonumber --trace
```

Run reconstruction from a fully unbiased start (`Any`), without scheme-seeded input narrowing:

```bash
cargo run -- --unbiased
```

Trace all bundled examples:

```bash
cargo run -- --trace
```

Trace output includes:

- candidate input type before/after each iteration
- inferred output type
- sample count
- whether refinement happened
- failing counterexample input
- runtime error
- subtracted type
- marked subset (`Subset<T>`) when only part of a type is rejected

## Latest Example Results

Generated from current `cargo run` output.

| Program                    | Inferred Scheme                                                                        | Reconstructed Input Type                                                        | Converged |
| -------------------------- | -------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------- | --------- |
| `identity`                 | `T0 -> T0`                                                                             | `Any`                                                                           | `true`    |
| `add_self`                 | `Number -> Number`                                                                     | `Number`                                                                        | `true`    |
| `alt_numeric`              | `Object{n: Number, ..R0} -> Number`                                                    | `Object{n: Number, ..Any}`                                                      | `true`    |
| `length`                   | `Array<E1> \| Object \| String -> Number`                                              | `Array<Any> \| Object \| String`                                                | `true`    |
| `first`                    | `NonEmptyArray<E1> -> E1`                                                              | `NonEmptyArray<Any>`                                                            | `true`    |
| `last`                     | `NonEmptyArray<E1> -> E1`                                                              | `NonEmptyArray<Any>`                                                            | `true`    |
| `index0`                   | `NonEmptyArray<I1> -> I1`                                                              | `NonEmptyArray<Any>`                                                            | `true`    |
| `nested_name_length`       | `Object{user: Object{name: Array<E3> \| Object \| String, ..R1}, ..R0} -> Number`      | `Object{user: Object{name: Array<Any> \| Object \| String, ..Any}, ..Any}`      | `true`    |
| `sum_nested_metrics`       | `Object{metrics: Object{a: Number, b: Number, ..R1}, ..R0} -> Number`                  | `Object{metrics: Object{a: Number, b: Number, ..Any}, ..Any}`                   | `true`    |
| `count_items_field`        | `Object{payload: Object{items: Array<E3> \| Object \| String, ..R1}, ..R0} -> Number`  | `Object{payload: Object{items: Array<Any> \| Object \| String, ..Any}, ..Any}`  | `true`    |
| `options_count`            | `Object{config: Object{options: Array<E3> \| Object \| String, ..R1}, ..R0} -> Number` | `Object{config: Object{options: Array<Any> \| Object \| String, ..Any}, ..Any}` | `true`    |
| `if_score_positive`        | `Object{score: F2, ..R0} -> F2 \| Number`                                              | `Object{score: Any, ..Any}`                                                     | `true`    |
| `range_check`              | `Object{score: F2, ..R0} -> Bool`                                                      | `Object{score: Any, ..Any}`                                                     | `true`    |
| `object_constructor`       | `Object{a: Number, b: Number, ..R0} -> Object{ok: Bool, sum: Number}`                  | `Object{a: Number, b: Number, ..Any}`                                           | `true`    |
| `array_constructor`        | `Object{x: Number, y: Number, ..R0} -> Tuple<F1, F2, Number>`                          | `Object{x: Number, y: Number, ..Any}`                                           | `true`    |
| `keys_then_length`         | `Object{meta: Object, ..R0} -> Number`                                                 | `Object{meta: Object, ..Any}`                                                   | `true`    |
| `map_increment`            | `Array<Number> -> Array<Number>`                                                       | `Array<Number>`                                                                 | `true`    |
| `map_select_positive`      | `Array<M1> -> Array<M1 \| Null>`                                                       | `Array<Any>`                                                                    | `true`    |
| `has_config`               | `Array<Any> \| Object -> Bool`                                                         | `Array<Any> \| Object`                                                          | `true`    |
| `tags_contains_subset`     | `Object{tags: F1, ..R0} -> Bool`                                                       | `Object{tags: Any, ..Any}`                                                      | `true`    |
| `name_prefix_or_suffix`    | `Object{name: String, ..R0} -> Bool`                                                   | `Object{name: String, ..Any}`                                                   | `true`    |
| `contains_object_fragment` | `T0 -> Bool`                                                                           | `Any`                                                                           | `true`    |
| `dynamic_lookup_fallback`  | `T0 -> Any`                                                                            | `Any`                                                                           | `true`    |
| `safe_nested_age`          | `T0 -> Any`                                                                            | `Any`                                                                           | `true`    |
| `safe_tonumber_try`        | `Object{raw: Number \| String, ..R0} -> Number`                                        | `Object{raw: Number \| String, ..Any}`                                          | `true`    |
| `sum_values_add`           | `Object{values: Array<A2>, ..R0} -> Any`                                               | `Object{values: Array<Any>, ..Any}`                                             | `true`    |
| `max_score_try`            | `Object{scores: NonEmptyArray<X2>, ..R0} -> X2 \| Null`                                | `Object{scores: NonEmptyArray<Any>, ..Any}`                                     | `true`    |
| `csv_split`                | `Object{csv: String, ..R0} -> Array<String>`                                           | `Object{csv: String, ..Any}`                                                    | `true`    |
| `tags_join`                | `Object{tags: Array<Bool \| Null \| Number \| String>, ..R0} -> String`                | `Object{tags: Array<Bool \| Null \| Number \| String>, ..Any}`                  | `true`    |
| `sorted_top_score`         | `Object{scores: Array<S3>, ..R0} -> I5 \| Null`                                        | `Object{scores: Array<Any>, ..Any}`                                             | `true`    |
| `has_first_item`           | `NonEmptyArray<I1> -> I1 \| Null`                                                      | `NonEmptyArray<Any>`                                                            | `true`    |
| `floating_threshold`       | `Object{latency: F1, ..R0} -> Bool`                                                    | `Object{latency: Any, ..Any}`                                                   | `true`    |
| `normalized_price`         | `Object{price: F1, ..R0} -> Number`                                                    | `Object{price: Any, ..Any}`                                                     | `true`    |
| `ceil_latency`             | `Object{latency: F1, ..R0} -> Number`                                                  | `Object{latency: Any, ..Any}`                                                   | `true`    |
| `type_probe`               | `T0 -> String`                                                                         | `Any`                                                                           | `true`    |
| `double_pipeline`          | `Object{profile: Object{alias: Array<E3> \| Object \| String, ..R1}, ..R0} -> Number`  | `Object{profile: Object{alias: Array<Any> \| Object \| String, ..Any}, ..Any}`  | `true`    |
