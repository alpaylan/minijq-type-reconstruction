use crate::minijq::ast::Expr;
use crate::minijq::parser::parse_expr;

#[derive(Clone, Debug)]
pub struct ExampleProgram {
    pub name: &'static str,
    pub description: &'static str,
    pub source: &'static str,
    pub expr: Expr,
}

pub fn all_examples() -> Vec<ExampleProgram> {
    let examples = [
        ("identity", "Identity filter (polymorphic)", "."),
        ("add_self", "Numeric self addition", ". + ."),
        ("alt_numeric", "Fallback + arithmetic", "(.n // 0) * 2"),
        ("length", "Length on string/array/object", "length(.)"),
        ("first", "First element of an array", "first(.)"),
        ("last", "Last element of an array", "last(.)"),
        ("index0", "Index zero", ".[0]"),
        (
            "nested_name_length",
            "Read nested object key and compute length",
            ".user.name | length(.)",
        ),
        (
            "sum_nested_metrics",
            "Sum two nested numeric fields",
            ".metrics.a + .metrics.b",
        ),
        (
            "count_items_field",
            "Length of nested collection field",
            "length(.payload.items)",
        ),
        (
            "options_count",
            "Count nested options collection",
            "length(.config.options)",
        ),
        (
            "if_score_positive",
            "Conditional on numeric comparison",
            "if .score > 0 then .score else 0 end",
        ),
        (
            "range_check",
            "Boolean logic with chained comparisons",
            ".score >= 10 and .score < 20",
        ),
        (
            "object_constructor",
            "Build object from computations",
            "{sum: (.a + .b), ok: (.a > .b)}",
        ),
        (
            "array_constructor",
            "Build tuple-like array",
            "[.x, .y, (.x + .y)]",
        ),
        (
            "keys_then_length",
            "Object keys then cardinality",
            ".meta | keys(.) | length(.)",
        ),
        (
            "map_increment",
            "Map arithmetic over array elements",
            "map(. + 1)",
        ),
        (
            "map_select_positive",
            "Map with nested select filter",
            "map(select(. > 0))",
        ),
        (
            "has_config",
            "Check whether an object has a required key",
            "has(\"config\")",
        ),
        (
            "tags_contains_subset",
            "Subset containment over arrays",
            ".tags | contains([\"core\", \"beta\"])",
        ),
        (
            "name_prefix_or_suffix",
            "String prefix/suffix predicate combination",
            ".name | startswith(\"pre\") or endswith(\"fix\")",
        ),
        (
            "contains_object_fragment",
            "Recursive object containment query",
            "contains({status: \"ok\", flags: {beta: true}})",
        ),
        (
            "dynamic_lookup_fallback",
            "Lookup object field using a dynamic key with optional fallback",
            ".data[.key]? // \"missing\"",
        ),
        (
            "safe_nested_age",
            "Optional nested field access with numeric default",
            ".user.profile.age? // 0",
        ),
        (
            "safe_tonumber_try",
            "Try/catch numeric conversion from dynamic field",
            "try (.raw | tonumber(.)) catch 0",
        ),
        (
            "strict_tonumber",
            "Strict conversion without catch to show refinement",
            "tonumber(.)",
        ),
        (
            "sum_values_add",
            "Aggregate array values with add and recover from incompatible inputs",
            "try (.values | add(.)) catch null",
        ),
        (
            "max_score_try",
            "Find maximum score with graceful empty handling",
            "try (.scores | max(.)) catch null",
        ),
        (
            "csv_split",
            "Split CSV string into an array",
            ".csv | split(\",\")",
        ),
        (
            "tags_join",
            "Join tag array into a comma separated string",
            ".tags | join(\",\")",
        ),
        (
            "sorted_top_score",
            "Sort, reverse, and fetch top score when non-empty",
            ".scores | if length(.) > 0 then sort(.) | reverse(.) | .[0] else null end",
        ),
        (
            "has_first_item",
            "Guard an index access with has",
            "if has(0) then .[0] else null end",
        ),
        (
            "floating_threshold",
            "Float literal in arithmetic comparison",
            ".latency < 10.5",
        ),
        (
            "normalized_price",
            "Convert dynamic number-like input and normalize sign",
            "(.price | tonumber(.)?) // 0 | abs(.)",
        ),
        (
            "ceil_latency",
            "Convert then ceil numeric value",
            "(.latency | tonumber(.)?) // 0 | ceil(.)",
        ),
        ("type_probe", "Report dynamic type name", "type(.)"),
        (
            "bool_guard_or_error",
            "Explicit error guard (inspired by tjq usage)",
            "if . == true or . == false then 1 else error end",
        ),
        (
            "bool_guard_and_error",
            "Explicit error guard (inspired by tjq usage)",
            "if . == true or . == false then error else \"alp\" end",
        ),
        (
            "isnumber",
            "Explicit error guard (inspired by tjq usage)",
            "if . > true and . < \"\" then . else error end",
        ),
        (
            "isstring",
            "Explicit error guard (inspired by tjq usage)",
            "if . >= \"\"  and . < [] then . else error end",
        ),
        (
            "isarray",
            "Explicit error guard (inspired by tjq usage)",
            "if . >= []  and . < {} then . else error end",
        ),
        (
            "isobject",
            "Explicit error guard (inspired by tjq usage)",
            "if . >= {} then . else error end",
        ),
        (
            "tjq_word_reverse",
            "Port of tjq short/test4 word-reversal pipeline",
            ". | split(\" \") | map(split(\"\")) | reverse | map(reverse) | map(join(\"\")) | join(\" \")",
        ),
        (
            "tjq_recipe_projection",
            "Adapted from tjq short/test1 ingredient projection",
            "[ .name, (.ingredients | length), (.ingredients | map(select(.item == \"sugar\") | .amount.quantity)) ]",
        ),
        (
            "double_pipeline",
            "Compose field lookups and builtin calls",
            ".profile.alias | length(.) + 1",
        ),
    ];

    examples
        .into_iter()
        .map(|(name, description, source)| ExampleProgram {
            name,
            description,
            source,
            expr: parse_expr(source)
                .unwrap_or_else(|err| panic!("invalid builtin example `{source}`: {err}")),
        })
        .collect()
}
