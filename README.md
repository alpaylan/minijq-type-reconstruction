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
5. If a dynamic type error occurs, subtract the failing region from the candidate type.
6. Repeat until stable.

## Current Progress

Implemented:

- A `minijq` AST, parser, evaluator, type system, random generator, and reconstruction loop.
- Structural JSON-like types with:
  - `Any`, `Never`, primitives.
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

Language features currently supported:

- navigation: `.field`, `.[idx]`, dynamic lookup `.[expr]`
- postfix optional error suppression: `expr?`
- `try expr catch expr`
- literals: numbers (including floats), strings, booleans, null, arrays, objects
- operators: `+ - * /`, comparisons, `and`, `or`, `//`, unary `not` and unary `-`
- conditionals: `if ... then ... else ... end`
- piping: `lhs | rhs`

Builtins currently supported:

- `length`, `first`, `last`, `keys`, `type`
- `map`, `select`
- `has`, `contains`, `startswith`, `endswith`
- `reverse`, `sort`
- `tostring`, `tonumber`, `abs`, `floor`, `ceil`
- `add`, `min`, `max`, `split`, `join`

Validation status:

- `cargo test`: 92 passed, 0 failed.
- `cargo run`: all bundled examples converge.

## Run

```bash
cargo test
cargo run
```

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
| `tags_join`                | `Object{tags: Array<String>, ..R0} -> String`                                          | `Object{tags: Array<String>, ..Any}`                                            | `true`    |
| `sorted_top_score`         | `Object{scores: Array<S3>, ..R0} -> I5 \| Null`                                        | `Object{scores: Array<Any>, ..Any}`                                             | `true`    |
| `has_first_item`           | `NonEmptyArray<I1> -> I1 \| Null`                                                      | `NonEmptyArray<Any>`                                                            | `true`    |
| `floating_threshold`       | `Object{latency: F1, ..R0} -> Bool`                                                    | `Object{latency: Any, ..Any}`                                                   | `true`    |
| `normalized_price`         | `Object{price: F1, ..R0} -> Number`                                                    | `Object{price: Any, ..Any}`                                                     | `true`    |
| `ceil_latency`             | `Object{latency: F1, ..R0} -> Number`                                                  | `Object{latency: Any, ..Any}`                                                   | `true`    |
| `type_probe`               | `T0 -> String`                                                                         | `Any`                                                                           | `true`    |
| `double_pipeline`          | `Object{profile: Object{alias: Array<E3> \| Object \| String, ..R1}, ..R0} -> Number`  | `Object{profile: Object{alias: Array<Any> \| Object \| String, ..Any}, ..Any}`  | `true`    |
