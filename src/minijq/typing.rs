use crate::minijq::ast::{BinaryOp, Expr, UnaryOp};
use crate::minijq::types::{ObjectShape, RowTail, Type};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Builtin {
    Error,
    Length,
    First,
    Last,
    Keys,
    TypeName,
    Map,
    Select,
    Has,
    Contains,
    StartsWith,
    EndsWith,
    Reverse,
    Sort,
    ToString,
    ToNumber,
    Abs,
    Floor,
    Ceil,
    Add,
    Min,
    Max,
    Split,
    Join,
}

impl Builtin {
    pub fn name(&self) -> &'static str {
        match self {
            Builtin::Error => "error",
            Builtin::Length => "length",
            Builtin::First => "first",
            Builtin::Last => "last",
            Builtin::Keys => "keys",
            Builtin::TypeName => "type",
            Builtin::Map => "map",
            Builtin::Select => "select",
            Builtin::Has => "has",
            Builtin::Contains => "contains",
            Builtin::StartsWith => "startswith",
            Builtin::EndsWith => "endswith",
            Builtin::Reverse => "reverse",
            Builtin::Sort => "sort",
            Builtin::ToString => "tostring",
            Builtin::ToNumber => "tonumber",
            Builtin::Abs => "abs",
            Builtin::Floor => "floor",
            Builtin::Ceil => "ceil",
            Builtin::Add => "add",
            Builtin::Min => "min",
            Builtin::Max => "max",
            Builtin::Split => "split",
            Builtin::Join => "join",
        }
    }

    pub fn scheme(&self) -> TypeScheme {
        match self {
            Builtin::Error => TypeScheme {
                vars: vec!["T".to_string()],
                input: Type::Generic("T".to_string()),
                output: Type::Never,
            },
            Builtin::Length => TypeScheme {
                vars: vec!["T".to_string()],
                input: Type::union(vec![
                    Type::String,
                    Type::Array(Box::new(Type::Generic("T".to_string()))),
                    Type::Object(ObjectShape::open_any()),
                ]),
                output: Type::Number,
            },
            Builtin::First => TypeScheme {
                vars: vec!["T".to_string()],
                input: Type::NonEmptyArray(Box::new(Type::Generic("T".to_string()))),
                output: Type::Generic("T".to_string()),
            },
            Builtin::Last => TypeScheme {
                vars: vec!["T".to_string()],
                input: Type::NonEmptyArray(Box::new(Type::Generic("T".to_string()))),
                output: Type::Generic("T".to_string()),
            },
            Builtin::Keys => TypeScheme {
                vars: vec!["R".to_string()],
                input: Type::Object(ObjectShape::open_any()),
                output: Type::Array(Box::new(Type::String)),
            },
            Builtin::TypeName => TypeScheme {
                vars: vec!["T".to_string()],
                input: Type::Generic("T".to_string()),
                output: Type::String,
            },
            Builtin::Map => TypeScheme {
                vars: vec!["T".to_string()],
                input: Type::Array(Box::new(Type::Generic("T".to_string()))),
                output: Type::Array(Box::new(Type::Any)),
            },
            Builtin::Select => TypeScheme {
                vars: vec!["T".to_string()],
                input: Type::Generic("T".to_string()),
                output: Type::union(vec![Type::Generic("T".to_string()), Type::Null]),
            },
            Builtin::Has => TypeScheme {
                vars: vec![],
                input: Type::union(vec![
                    Type::Object(ObjectShape::open_any()),
                    Type::Array(Box::new(Type::Any)),
                ]),
                output: Type::Bool,
            },
            Builtin::Contains => TypeScheme {
                vars: vec!["T".to_string()],
                input: Type::Generic("T".to_string()),
                output: Type::Bool,
            },
            Builtin::StartsWith => TypeScheme {
                vars: vec![],
                input: Type::String,
                output: Type::Bool,
            },
            Builtin::EndsWith => TypeScheme {
                vars: vec![],
                input: Type::String,
                output: Type::Bool,
            },
            Builtin::Reverse => TypeScheme {
                vars: vec!["T".to_string()],
                input: Type::Array(Box::new(Type::Generic("T".to_string()))),
                output: Type::Array(Box::new(Type::Generic("T".to_string()))),
            },
            Builtin::Sort => TypeScheme {
                vars: vec!["T".to_string()],
                input: Type::Array(Box::new(Type::Generic("T".to_string()))),
                output: Type::Array(Box::new(Type::Generic("T".to_string()))),
            },
            Builtin::ToString => TypeScheme {
                vars: vec!["T".to_string()],
                input: Type::Generic("T".to_string()),
                output: Type::String,
            },
            Builtin::ToNumber => TypeScheme {
                vars: vec![],
                input: Type::union(vec![Type::Number, Type::String]),
                output: Type::Number,
            },
            Builtin::Abs => TypeScheme {
                vars: vec![],
                input: Type::Number,
                output: Type::Number,
            },
            Builtin::Floor => TypeScheme {
                vars: vec![],
                input: Type::Number,
                output: Type::Number,
            },
            Builtin::Ceil => TypeScheme {
                vars: vec![],
                input: Type::Number,
                output: Type::Number,
            },
            Builtin::Add => TypeScheme {
                vars: vec!["T".to_string()],
                input: Type::Array(Box::new(Type::Generic("T".to_string()))),
                output: Type::Any,
            },
            Builtin::Min => TypeScheme {
                vars: vec!["T".to_string()],
                input: Type::NonEmptyArray(Box::new(Type::Generic("T".to_string()))),
                output: Type::Generic("T".to_string()),
            },
            Builtin::Max => TypeScheme {
                vars: vec!["T".to_string()],
                input: Type::NonEmptyArray(Box::new(Type::Generic("T".to_string()))),
                output: Type::Generic("T".to_string()),
            },
            Builtin::Split => TypeScheme {
                vars: vec![],
                input: Type::String,
                output: Type::Array(Box::new(Type::String)),
            },
            Builtin::Join => TypeScheme {
                vars: vec![],
                input: Type::Array(Box::new(Type::union(vec![
                    Type::String,
                    Type::Number,
                    Type::Bool,
                    Type::Null,
                ]))),
                output: Type::String,
            },
        }
    }

    pub fn expected_input_type(&self) -> Type {
        self.scheme().input.erase_generics()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeScheme {
    pub vars: Vec<String>,
    pub input: Type,
    pub output: Type,
}

impl TypeScheme {
    pub fn instantiate_output(&self, actual_input: &Type) -> Option<Type> {
        match actual_input {
            Type::Union(items) => {
                let mut outputs = Vec::new();
                for item in items {
                    if let Some(out) = self.instantiate_output_single(item) {
                        outputs.push(out);
                    }
                }
                if outputs.is_empty() {
                    None
                } else {
                    Some(Type::union(outputs))
                }
            }
            other => self.instantiate_output_single(other),
        }
    }

    fn instantiate_output_single(&self, actual_input: &Type) -> Option<Type> {
        let mut bindings = HashMap::new();
        if !unify_pattern(&self.input, actual_input, &mut bindings) {
            return None;
        }
        Some(substitute_generics(&self.output, &bindings))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PredicateRefinement {
    pub input_domain: Type,
    pub true_input: Type,
    pub false_input: Type,
    pub unknown_input: Type,
}

impl PredicateRefinement {
    pub fn has_information(&self) -> bool {
        !self.true_input.is_never() || !self.false_input.is_never()
    }

    pub fn is_total(&self) -> bool {
        self.unknown_input.is_never()
    }

    pub fn pretty(&self) -> String {
        let mut clauses = Vec::new();
        if !self.true_input.is_never() {
            clauses.push(format!("{} -> Bool<true>", self.true_input));
        }
        if !self.false_input.is_never() {
            if self.is_total() {
                let not_true = self.input_domain.subtract(&self.true_input).normalize();
                if not_true == self.false_input && !self.true_input.is_never() {
                    clauses.push(format!("Not<{}> -> Bool<false>", self.true_input));
                } else {
                    clauses.push(format!("{} -> Bool<false>", self.false_input));
                }
            } else {
                clauses.push(format!("{} -> Bool<false>", self.false_input));
            }
        }
        if !self.unknown_input.is_never() {
            clauses.push(format!("{} -> Bool", self.unknown_input));
        }
        clauses.join(" ; ")
    }
}

pub fn infer_predicate_refinement(expr: &Expr, input_domain: &Type) -> Option<PredicateRefinement> {
    let output = infer_expr_type(expr, input_domain).normalize();
    if !output.is_subtype_of(&Type::Bool) {
        return None;
    }

    let partitions = vec![
        Type::Null,
        Type::BoolLiteral(true),
        Type::BoolLiteral(false),
        Type::Number,
        Type::String,
        Type::Array(Box::new(Type::Any)),
        Type::Object(ObjectShape::open_any()),
    ];

    let mut true_inputs = Vec::new();
    let mut false_inputs = Vec::new();
    let mut unknown_inputs = Vec::new();

    for partition in partitions {
        let slice = input_domain.intersect(&partition).normalize();
        if slice.is_never() {
            continue;
        }

        let out = infer_expr_type(expr, &slice).normalize();
        match out {
            Type::BoolLiteral(true) => true_inputs.push(slice),
            Type::BoolLiteral(false) => false_inputs.push(slice),
            _ => unknown_inputs.push(slice),
        }
    }

    let true_input = canonicalize_predicate_domain(Type::union(true_inputs));
    let false_input = canonicalize_predicate_domain(Type::union(false_inputs));
    let unknown_input = canonicalize_predicate_domain(Type::union(unknown_inputs));

    Some(PredicateRefinement {
        input_domain: input_domain.clone().normalize(),
        true_input,
        false_input,
        unknown_input,
    })
}

fn canonicalize_predicate_domain(ty: Type) -> Type {
    match ty.normalize() {
        Type::Union(items) => {
            let mut has_true = false;
            let mut has_false = false;
            let mut others = Vec::new();

            for item in items {
                match item {
                    Type::BoolLiteral(true) => has_true = true,
                    Type::BoolLiteral(false) => has_false = true,
                    other => others.push(other),
                }
            }

            if has_true && has_false {
                others.push(Type::Bool);
            } else {
                if has_true {
                    others.push(Type::BoolLiteral(true));
                }
                if has_false {
                    others.push(Type::BoolLiteral(false));
                }
            }

            Type::union(others)
        }
        other => other,
    }
}

pub fn infer_expr_type(expr: &Expr, input: &Type) -> Type {
    match expr {
        Expr::Identity => input.clone(),
        Expr::Literal(value) => Type::from_json_value(value),
        Expr::Pipe(lhs, rhs) => {
            let mid = infer_expr_type(lhs, input);
            if mid.is_never() {
                Type::Never
            } else {
                infer_expr_type(rhs, &mid)
            }
        }
        Expr::Binary(op, lhs, rhs) => {
            let left = infer_expr_type(lhs, input);
            let right = infer_expr_type(rhs, input);
            infer_binary_type(*op, left, right)
        }
        Expr::Unary(op, inner) => {
            let t = infer_expr_type(inner, input);
            infer_unary_type(*op, t)
        }
        Expr::Builtin(Builtin::Map, inner) => infer_map_type(inner, input),
        Expr::Builtin(Builtin::Select, inner) => {
            let pred_ty = infer_expr_type(inner, input);
            if pred_ty.is_never() || input.is_never() {
                Type::Never
            } else if let Some(refinement) = infer_predicate_refinement(inner, input) {
                Type::union(vec![refinement.true_input, refinement.unknown_input]).normalize()
            } else {
                input.clone()
            }
        }
        Expr::Builtin(Builtin::Has, inner) => infer_has_type(inner, input),
        Expr::Builtin(Builtin::Contains, inner) => infer_contains_type(inner, input),
        Expr::Builtin(Builtin::StartsWith, inner) | Expr::Builtin(Builtin::EndsWith, inner) => {
            infer_string_predicate_type(inner, input)
        }
        Expr::Builtin(Builtin::Reverse, inner) => infer_reverse_type(inner, input),
        Expr::Builtin(Builtin::Sort, inner) => infer_sort_type(inner, input),
        Expr::Builtin(Builtin::Add, inner) => infer_add_builtin_type(inner, input),
        Expr::Builtin(Builtin::Min, inner) | Expr::Builtin(Builtin::Max, inner) => {
            infer_min_max_builtin_type(inner, input)
        }
        Expr::Builtin(Builtin::Split, inner) => infer_split_builtin_type(inner, input),
        Expr::Builtin(Builtin::Join, inner) => infer_join_builtin_type(inner, input),
        Expr::Builtin(op, inner) => {
            let inner_type = infer_expr_type(inner, input);
            op.scheme()
                .instantiate_output(&inner_type)
                .unwrap_or(Type::Never)
        }
        Expr::Call(_, inner) => {
            let arg_ty = infer_expr_type(inner, input);
            if arg_ty.is_never() {
                Type::Never
            } else {
                Type::Any
            }
        }
        Expr::Field(inner, name) => field_output_type(&infer_expr_type(inner, input), name),
        Expr::Index(inner, idx) => index_output_type(&infer_expr_type(inner, input), *idx),
        Expr::Lookup(inner, key) => {
            let container_ty = infer_expr_type(inner, input);
            let key_ty = infer_expr_type(key, input);
            lookup_output_type(&container_ty, &key_ty, static_lookup_key(key))
        }
        Expr::Optional(inner) => {
            let out = infer_expr_type(inner, input);
            if out.is_never() {
                Type::Null
            } else {
                Type::union(vec![out, Type::Null])
            }
        }
        Expr::TryCatch {
            try_expr,
            catch_expr,
        } => {
            let try_ty = infer_expr_type(try_expr, input);
            let catch_ty = infer_expr_type(catch_expr, input);
            Type::union(vec![try_ty, catch_ty])
        }
        Expr::IfElse {
            cond: _,
            then_branch,
            else_branch,
        } => {
            let then_ty = infer_expr_type(then_branch, input);
            let else_ty = infer_expr_type(else_branch, input);
            Type::union(vec![then_ty, else_ty])
        }
        Expr::ArrayLiteral(items) => Type::Tuple(
            items
                .iter()
                .map(|item| infer_expr_type(item, input))
                .collect(),
        ),
        Expr::ObjectLiteral(fields) => {
            let mut out = BTreeMap::new();
            for (k, v) in fields {
                out.insert(k.clone(), infer_expr_type(v, input));
            }
            Type::Object(ObjectShape::closed(out))
        }
    }
}

fn infer_binary_type(op: BinaryOp, left: Type, right: Type) -> Type {
    if left.is_never() || right.is_never() {
        return Type::Never;
    }

    match op {
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
            if left.is_subtype_of(&Type::Number) && right.is_subtype_of(&Type::Number) {
                Type::Number
            } else {
                Type::Never
            }
        }
        BinaryOp::Eq | BinaryOp::Ne => {
            if left.intersect(&right).normalize().is_never() {
                return if matches!(op, BinaryOp::Eq) {
                    Type::BoolLiteral(false)
                } else {
                    Type::BoolLiteral(true)
                };
            }
            if let Some(eq) = literal_equality(&left, &right) {
                return if matches!(op, BinaryOp::Eq) {
                    Type::BoolLiteral(eq)
                } else {
                    Type::BoolLiteral(!eq)
                };
            }
            Type::Bool
        }
        BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => {
            infer_order_comparison_type(op, &left, &right)
        }
        BinaryOp::And => match (&left, &right) {
            (Type::BoolLiteral(false), _) | (_, Type::BoolLiteral(false)) => {
                Type::BoolLiteral(false)
            }
            (Type::BoolLiteral(true), rhs) => {
                if rhs.is_subtype_of(&Type::Bool) {
                    rhs.clone()
                } else {
                    Type::Bool
                }
            }
            (lhs, Type::BoolLiteral(true)) => {
                if lhs.is_subtype_of(&Type::Bool) {
                    lhs.clone()
                } else {
                    Type::Bool
                }
            }
            _ => Type::Bool,
        },
        BinaryOp::Or => match (&left, &right) {
            (Type::BoolLiteral(true), _) | (_, Type::BoolLiteral(true)) => Type::BoolLiteral(true),
            (Type::BoolLiteral(false), rhs) => {
                if rhs.is_subtype_of(&Type::Bool) {
                    rhs.clone()
                } else {
                    Type::Bool
                }
            }
            (lhs, Type::BoolLiteral(false)) => {
                if lhs.is_subtype_of(&Type::Bool) {
                    lhs.clone()
                } else {
                    Type::Bool
                }
            }
            _ => Type::Bool,
        },
        BinaryOp::Alt => {
            let narrowed_left = left.subtract(&Type::Null).normalize();
            Type::union(vec![narrowed_left, right])
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum ComparisonAtom {
    Null,
    Bool(Option<bool>),
    Number(Option<String>),
    String(Option<String>),
    Array(ArrayAtom),
    Object(ObjectAtom),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ArrayAtom {
    Any,
    EmptyLiteral,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ObjectAtom {
    Any,
    EmptyLiteral,
}

#[derive(Clone, Copy, Debug, Default)]
struct OutcomeSet {
    can_true: bool,
    can_false: bool,
}

impl OutcomeSet {
    fn record(&mut self, value: bool) {
        if value {
            self.can_true = true;
        } else {
            self.can_false = true;
        }
    }

    fn union_with(&mut self, other: OutcomeSet) {
        self.can_true |= other.can_true;
        self.can_false |= other.can_false;
    }
}

fn infer_order_comparison_type(op: BinaryOp, left: &Type, right: &Type) -> Type {
    let outcomes = compare_outcome_set(op, left, right);
    match (outcomes.can_true, outcomes.can_false) {
        (true, false) => Type::BoolLiteral(true),
        (false, true) => Type::BoolLiteral(false),
        (true, true) => Type::Bool,
        (false, false) => Type::Never,
    }
}

fn compare_outcome_set(op: BinaryOp, left: &Type, right: &Type) -> OutcomeSet {
    let left_atoms = comparison_atoms(left);
    let right_atoms = comparison_atoms(right);
    let mut outcomes = OutcomeSet::default();

    for left_atom in &left_atoms {
        for right_atom in &right_atoms {
            outcomes.union_with(compare_atom_pair(op, left_atom, right_atom));
            if outcomes.can_true && outcomes.can_false {
                return outcomes;
            }
        }
    }

    outcomes
}

fn comparison_atoms(ty: &Type) -> Vec<ComparisonAtom> {
    fn push_unique(out: &mut Vec<ComparisonAtom>, atom: ComparisonAtom) {
        if !out.contains(&atom) {
            out.push(atom);
        }
    }

    let mut out = Vec::new();
    match ty {
        Type::Never => {}
        Type::Null => push_unique(&mut out, ComparisonAtom::Null),
        Type::Bool => push_unique(&mut out, ComparisonAtom::Bool(None)),
        Type::BoolLiteral(v) => push_unique(&mut out, ComparisonAtom::Bool(Some(*v))),
        Type::Number => push_unique(&mut out, ComparisonAtom::Number(None)),
        Type::NumberLiteral(text) => {
            push_unique(&mut out, ComparisonAtom::Number(Some(text.clone())))
        }
        Type::String => push_unique(&mut out, ComparisonAtom::String(None)),
        Type::StringLiteral(text) => {
            push_unique(&mut out, ComparisonAtom::String(Some(text.clone())))
        }
        Type::Array(_) | Type::NonEmptyArray(_) => {
            push_unique(&mut out, ComparisonAtom::Array(ArrayAtom::Any))
        }
        Type::Tuple(items) => {
            let atom = if items.is_empty() {
                ComparisonAtom::Array(ArrayAtom::EmptyLiteral)
            } else {
                ComparisonAtom::Array(ArrayAtom::Any)
            };
            push_unique(&mut out, atom);
        }
        Type::Object(shape) => {
            let atom = if shape.fields.is_empty() && matches!(shape.tail, RowTail::Closed) {
                ComparisonAtom::Object(ObjectAtom::EmptyLiteral)
            } else {
                ComparisonAtom::Object(ObjectAtom::Any)
            };
            push_unique(&mut out, atom);
        }
        Type::Any | Type::Generic(_) => {
            push_unique(&mut out, ComparisonAtom::Null);
            push_unique(&mut out, ComparisonAtom::Bool(None));
            push_unique(&mut out, ComparisonAtom::Number(None));
            push_unique(&mut out, ComparisonAtom::String(None));
            push_unique(&mut out, ComparisonAtom::Array(ArrayAtom::Any));
            push_unique(&mut out, ComparisonAtom::Object(ObjectAtom::Any));
        }
        Type::Union(items) => {
            for item in items {
                for atom in comparison_atoms(item) {
                    push_unique(&mut out, atom);
                }
            }
        }
    }

    out
}

fn compare_atom_pair(op: BinaryOp, left: &ComparisonAtom, right: &ComparisonAtom) -> OutcomeSet {
    let left_rank = comparison_atom_rank(left);
    let right_rank = comparison_atom_rank(right);
    if left_rank != right_rank {
        return compare_known_ordering(op, left_rank.cmp(&right_rank));
    }

    match (left, right) {
        (ComparisonAtom::Null, ComparisonAtom::Null) => {
            compare_known_ordering(op, std::cmp::Ordering::Equal)
        }
        (ComparisonAtom::Bool(left_value), ComparisonAtom::Bool(right_value)) => {
            compare_bool_domains(op, *left_value, *right_value)
        }
        (ComparisonAtom::Number(Some(left_text)), ComparisonAtom::Number(Some(right_text))) => {
            let left_num = left_text.parse::<f64>().ok();
            let right_num = right_text.parse::<f64>().ok();
            match (left_num, right_num) {
                (Some(l), Some(r)) => l
                    .partial_cmp(&r)
                    .map(|ord| compare_known_ordering(op, ord))
                    .unwrap_or_else(compare_unknown_ordering),
                _ => compare_unknown_ordering(),
            }
        }
        (ComparisonAtom::String(Some(left_text)), ComparisonAtom::String(Some(right_text))) => {
            compare_known_ordering(op, left_text.cmp(right_text))
        }
        (ComparisonAtom::Array(left_atom), ComparisonAtom::Array(right_atom)) => {
            compare_array_domains(op, *left_atom, *right_atom)
        }
        (ComparisonAtom::Object(left_atom), ComparisonAtom::Object(right_atom)) => {
            compare_object_domains(op, *left_atom, *right_atom)
        }
        (ComparisonAtom::String(None), ComparisonAtom::String(Some(right_text))) => {
            if right_text.is_empty() {
                compare_string_domain_against_empty_literal(op)
            } else {
                compare_unknown_ordering()
            }
        }
        (ComparisonAtom::String(Some(left_text)), ComparisonAtom::String(None)) => {
            if left_text.is_empty() {
                compare_empty_literal_against_string_domain(op)
            } else {
                compare_unknown_ordering()
            }
        }
        (ComparisonAtom::String(None), ComparisonAtom::String(None))
        | (ComparisonAtom::Number(_), ComparisonAtom::Number(_)) => compare_unknown_ordering(),
        _ => compare_unknown_ordering(),
    }
}

fn comparison_atom_rank(atom: &ComparisonAtom) -> u8 {
    match atom {
        ComparisonAtom::Null => 0,
        ComparisonAtom::Bool(_) => 1,
        ComparisonAtom::Number(_) => 2,
        ComparisonAtom::String(_) => 3,
        ComparisonAtom::Array(_) => 4,
        ComparisonAtom::Object(_) => 5,
    }
}

fn compare_known_ordering(op: BinaryOp, ord: std::cmp::Ordering) -> OutcomeSet {
    let mut out = OutcomeSet::default();
    let value = match op {
        BinaryOp::Lt => ord.is_lt(),
        BinaryOp::Lte => ord.is_le(),
        BinaryOp::Gt => ord.is_gt(),
        BinaryOp::Gte => ord.is_ge(),
        _ => false,
    };
    out.record(value);
    out
}

fn compare_unknown_ordering() -> OutcomeSet {
    OutcomeSet {
        can_true: true,
        can_false: true,
    }
}

fn compare_bool_domains(op: BinaryOp, left: Option<bool>, right: Option<bool>) -> OutcomeSet {
    let left_values = left.map_or_else(|| vec![false, true], |v| vec![v]);
    let right_values = right.map_or_else(|| vec![false, true], |v| vec![v]);

    let mut out = OutcomeSet::default();
    for lhs in left_values {
        for rhs in &right_values {
            out.record(match op {
                BinaryOp::Lt => !lhs & *rhs,
                BinaryOp::Lte => lhs <= *rhs,
                BinaryOp::Gt => lhs & !*rhs,
                BinaryOp::Gte => lhs >= *rhs,
                _ => false,
            });
        }
    }
    out
}

fn compare_array_domains(op: BinaryOp, left: ArrayAtom, right: ArrayAtom) -> OutcomeSet {
    match (left, right) {
        (ArrayAtom::EmptyLiteral, ArrayAtom::EmptyLiteral) => {
            compare_known_ordering(op, std::cmp::Ordering::Equal)
        }
        (ArrayAtom::Any, ArrayAtom::EmptyLiteral) => match op {
            BinaryOp::Lt => OutcomeSet {
                can_true: false,
                can_false: true,
            },
            BinaryOp::Lte => compare_unknown_ordering(),
            BinaryOp::Gt => compare_unknown_ordering(),
            BinaryOp::Gte => OutcomeSet {
                can_true: true,
                can_false: false,
            },
            _ => compare_unknown_ordering(),
        },
        (ArrayAtom::EmptyLiteral, ArrayAtom::Any) => match op {
            BinaryOp::Lt => compare_unknown_ordering(),
            BinaryOp::Lte => OutcomeSet {
                can_true: true,
                can_false: false,
            },
            BinaryOp::Gt => OutcomeSet {
                can_true: false,
                can_false: true,
            },
            BinaryOp::Gte => compare_unknown_ordering(),
            _ => compare_unknown_ordering(),
        },
        (ArrayAtom::Any, ArrayAtom::Any) => compare_unknown_ordering(),
    }
}

fn compare_object_domains(op: BinaryOp, left: ObjectAtom, right: ObjectAtom) -> OutcomeSet {
    match (left, right) {
        (ObjectAtom::EmptyLiteral, ObjectAtom::EmptyLiteral) => {
            compare_known_ordering(op, std::cmp::Ordering::Equal)
        }
        (ObjectAtom::Any, ObjectAtom::EmptyLiteral) => match op {
            BinaryOp::Lt => OutcomeSet {
                can_true: false,
                can_false: true,
            },
            BinaryOp::Lte => compare_unknown_ordering(),
            BinaryOp::Gt => compare_unknown_ordering(),
            BinaryOp::Gte => OutcomeSet {
                can_true: true,
                can_false: false,
            },
            _ => compare_unknown_ordering(),
        },
        (ObjectAtom::EmptyLiteral, ObjectAtom::Any) => match op {
            BinaryOp::Lt => compare_unknown_ordering(),
            BinaryOp::Lte => OutcomeSet {
                can_true: true,
                can_false: false,
            },
            BinaryOp::Gt => OutcomeSet {
                can_true: false,
                can_false: true,
            },
            BinaryOp::Gte => compare_unknown_ordering(),
            _ => compare_unknown_ordering(),
        },
        (ObjectAtom::Any, ObjectAtom::Any) => compare_unknown_ordering(),
    }
}

fn compare_string_domain_against_empty_literal(op: BinaryOp) -> OutcomeSet {
    match op {
        BinaryOp::Lt => OutcomeSet {
            can_true: false,
            can_false: true,
        },
        BinaryOp::Lte => compare_unknown_ordering(),
        BinaryOp::Gt => compare_unknown_ordering(),
        BinaryOp::Gte => OutcomeSet {
            can_true: true,
            can_false: false,
        },
        _ => compare_unknown_ordering(),
    }
}

fn compare_empty_literal_against_string_domain(op: BinaryOp) -> OutcomeSet {
    match op {
        BinaryOp::Lt => compare_unknown_ordering(),
        BinaryOp::Lte => OutcomeSet {
            can_true: true,
            can_false: false,
        },
        BinaryOp::Gt => OutcomeSet {
            can_true: false,
            can_false: true,
        },
        BinaryOp::Gte => compare_unknown_ordering(),
        _ => compare_unknown_ordering(),
    }
}

fn literal_equality(left: &Type, right: &Type) -> Option<bool> {
    match (left, right) {
        (Type::Null, Type::Null) => Some(true),
        (Type::BoolLiteral(a), Type::BoolLiteral(b)) => Some(a == b),
        (Type::NumberLiteral(a), Type::NumberLiteral(b)) => Some(a == b),
        (Type::StringLiteral(a), Type::StringLiteral(b)) => Some(a == b),
        _ => None,
    }
}

fn infer_map_type(mapper: &Expr, input: &Type) -> Type {
    match input {
        Type::Array(elem) => Type::Array(Box::new(infer_expr_type(mapper, elem))),
        Type::NonEmptyArray(elem) => Type::NonEmptyArray(Box::new(infer_expr_type(mapper, elem))),
        Type::Tuple(items) => Type::Tuple(
            items
                .iter()
                .map(|item| infer_expr_type(mapper, item))
                .collect(),
        ),
        Type::Union(items) => Type::union(items.iter().map(|item| infer_map_type(mapper, item))),
        Type::Any => {
            let out = infer_expr_type(mapper, &Type::Any);
            Type::Array(Box::new(out))
        }
        _ => Type::Never,
    }
}

fn infer_has_type(key_expr: &Expr, input: &Type) -> Type {
    let key_ty = infer_expr_type(key_expr, input);
    if key_ty.is_never() {
        return Type::Never;
    }

    let object_like = !input
        .intersect(&Type::Object(ObjectShape::open_any()))
        .normalize()
        .is_never();
    let array_like = !input
        .intersect(&Type::Array(Box::new(Type::Any)))
        .normalize()
        .is_never();
    if object_like || array_like {
        Type::Bool
    } else {
        Type::Never
    }
}

fn infer_contains_type(arg_expr: &Expr, input: &Type) -> Type {
    let arg_ty = infer_expr_type(arg_expr, input);
    if input.is_never() || arg_ty.is_never() {
        Type::Never
    } else {
        Type::Bool
    }
}

fn infer_string_predicate_type(arg_expr: &Expr, input: &Type) -> Type {
    let arg_ty = infer_expr_type(arg_expr, input);
    if input.is_never() || arg_ty.is_never() {
        return Type::Never;
    }

    if !input.intersect(&Type::String).normalize().is_never()
        && !arg_ty.intersect(&Type::String).normalize().is_never()
    {
        Type::Bool
    } else {
        Type::Never
    }
}

fn infer_reverse_type(inner: &Expr, input: &Type) -> Type {
    let inner_ty = infer_expr_type(inner, input);
    match inner_ty {
        Type::Array(elem) => Type::Array(elem),
        Type::NonEmptyArray(elem) => Type::NonEmptyArray(elem),
        Type::Tuple(items) => Type::Tuple(items.into_iter().rev().collect()),
        Type::Union(items) => Type::union(items.into_iter().map(|item| match item {
            Type::Array(elem) => Type::Array(elem),
            Type::NonEmptyArray(elem) => Type::NonEmptyArray(elem),
            Type::Tuple(items) => Type::Tuple(items.into_iter().rev().collect()),
            _ => Type::Never,
        })),
        Type::Any => Type::Array(Box::new(Type::Any)),
        _ => Type::Never,
    }
}

fn infer_sort_type(inner: &Expr, input: &Type) -> Type {
    let inner_ty = infer_expr_type(inner, input);
    match inner_ty {
        Type::Array(elem) => Type::Array(elem),
        Type::NonEmptyArray(elem) => Type::NonEmptyArray(elem),
        Type::Tuple(items) => {
            let elem = Type::union(items);
            Type::Array(Box::new(elem))
        }
        Type::Union(items) => Type::union(items.into_iter().map(|item| match item {
            Type::Array(elem) => Type::Array(elem),
            Type::NonEmptyArray(elem) => Type::NonEmptyArray(elem),
            Type::Tuple(items) => {
                let elem = Type::union(items);
                Type::Array(Box::new(elem))
            }
            _ => Type::Never,
        })),
        Type::Any => Type::Array(Box::new(Type::Any)),
        _ => Type::Never,
    }
}

fn infer_add_builtin_type(inner: &Expr, input: &Type) -> Type {
    let inner_ty = infer_expr_type(inner, input);
    infer_add_container_type(inner_ty)
}

fn infer_add_container_type(inner_ty: Type) -> Type {
    match inner_ty {
        Type::Array(elem) | Type::NonEmptyArray(elem) => infer_add_over_element(elem.as_ref()),
        Type::Tuple(items) => {
            if items.is_empty() {
                Type::Null
            } else {
                let mut acc = infer_add_over_element(&items[0]);
                if acc.is_never() {
                    return Type::Never;
                }
                for item in items.iter().skip(1) {
                    acc = add_outputs(acc, infer_add_over_element(item));
                    if acc.is_never() {
                        return Type::Never;
                    }
                }
                acc
            }
        }
        Type::Union(items) => Type::union(items.into_iter().map(infer_add_container_type)),
        Type::Any => Type::Any,
        _ => Type::Never,
    }
}

fn infer_add_over_element(elem: &Type) -> Type {
    match elem {
        Type::Number => Type::Number,
        Type::String => Type::String,
        Type::Array(inner) => Type::Array(inner.clone()),
        Type::NonEmptyArray(inner) => Type::Array(inner.clone()),
        Type::Object(_) => Type::Object(ObjectShape::open_any()),
        Type::Union(items) => {
            let mut out = Vec::new();
            for item in items {
                let t = infer_add_over_element(item);
                if t.is_never() {
                    continue;
                }
                out.push(t);
            }
            Type::union(out)
        }
        Type::Any => Type::Any,
        _ => Type::Never,
    }
}

fn add_outputs(lhs: Type, rhs: Type) -> Type {
    if lhs.is_never() || rhs.is_never() {
        return Type::Never;
    }
    if lhs == rhs {
        return lhs;
    }
    match (lhs, rhs) {
        (Type::Array(a), Type::Array(b)) => Type::Array(Box::new(Type::union(vec![*a, *b]))),
        (l, r) => Type::union(vec![l, r]),
    }
}

fn infer_min_max_builtin_type(inner: &Expr, input: &Type) -> Type {
    let inner_ty = infer_expr_type(inner, input);
    match inner_ty {
        Type::NonEmptyArray(elem) => elem.as_ref().clone(),
        Type::Tuple(items) => {
            if items.is_empty() {
                Type::Never
            } else {
                Type::union(items)
            }
        }
        Type::Union(items) => Type::union(items.into_iter().map(|item| match item {
            Type::NonEmptyArray(elem) => elem.as_ref().clone(),
            Type::Tuple(items) if !items.is_empty() => Type::union(items),
            _ => Type::Never,
        })),
        Type::Any => Type::Any,
        _ => Type::Never,
    }
}

fn infer_split_builtin_type(inner: &Expr, input: &Type) -> Type {
    let delim_ty = infer_expr_type(inner, input);
    if delim_ty.intersect(&Type::String).normalize().is_never()
        || input.intersect(&Type::String).normalize().is_never()
    {
        Type::Never
    } else {
        Type::Array(Box::new(Type::String))
    }
}

fn infer_join_builtin_type(inner: &Expr, input: &Type) -> Type {
    let sep_ty = infer_expr_type(inner, input);
    if sep_ty.intersect(&Type::String).normalize().is_never() {
        return Type::Never;
    }

    let arr_ty = Type::Array(Box::new(Type::union(vec![
        Type::String,
        Type::Number,
        Type::Bool,
        Type::Null,
    ])));
    if input.intersect(&arr_ty).normalize().is_never() {
        Type::Never
    } else {
        Type::String
    }
}

fn infer_unary_type(op: UnaryOp, inner: Type) -> Type {
    if inner.is_never() {
        return Type::Never;
    }

    match op {
        UnaryOp::Not => match inner {
            Type::BoolLiteral(v) => Type::BoolLiteral(!v),
            _ => Type::Bool,
        },
        UnaryOp::Neg => {
            if inner.is_subtype_of(&Type::Number) {
                Type::Number
            } else {
                Type::Never
            }
        }
    }
}

pub fn infer_expr_scheme(expr: &Expr) -> TypeScheme {
    let mut ctx = PolyInferCtx::default();
    let root = ctx.fresh_var("T");
    let output = infer_poly(expr, root.clone(), &mut ctx);
    ctx.solve();

    let input = ctx.resolve_type(&root).normalize();
    let output = ctx.resolve_type(&output).normalize();

    let mut vars = BTreeSet::new();
    collect_free_generics(&input, &mut vars);
    collect_free_generics(&output, &mut vars);

    TypeScheme {
        vars: vars.into_iter().collect(),
        input,
        output,
    }
}

fn infer_poly(expr: &Expr, input: Type, ctx: &mut PolyInferCtx) -> Type {
    match expr {
        Expr::Identity => input,
        Expr::Literal(value) => Type::from_json_value(value),
        Expr::Pipe(lhs, rhs) => {
            let mid = infer_poly(lhs, input, ctx);
            infer_poly(rhs, mid, ctx)
        }
        Expr::Binary(op, lhs, rhs) => {
            let left = infer_poly(lhs, input.clone(), ctx);
            let right = infer_poly(rhs, input, ctx);
            infer_poly_binary(*op, left, right, ctx)
        }
        Expr::Unary(op, inner) => {
            let inner_ty = infer_poly(inner, input, ctx);
            match op {
                UnaryOp::Not => Type::Bool,
                UnaryOp::Neg => {
                    ctx.add_constraint(inner_ty, Type::Number);
                    Type::Number
                }
            }
        }
        Expr::Builtin(Builtin::Error, inner) => {
            let _inner_ty = infer_poly(inner, input, ctx);
            Type::Never
        }
        Expr::Builtin(Builtin::First, inner) | Expr::Builtin(Builtin::Last, inner) => {
            let inner_ty = infer_poly(inner, input, ctx);
            let elem = ctx.fresh_var("E");
            ctx.add_constraint(inner_ty, Type::NonEmptyArray(Box::new(elem.clone())));
            elem
        }
        Expr::Builtin(Builtin::Length, inner) => {
            let inner_ty = infer_poly(inner, input, ctx);
            let elem = ctx.fresh_var("E");
            ctx.add_constraint(
                inner_ty,
                Type::union(vec![
                    Type::String,
                    Type::Array(Box::new(elem)),
                    Type::Object(ObjectShape::open_any()),
                ]),
            );
            Type::Number
        }
        Expr::Builtin(Builtin::Keys, inner) => {
            let inner_ty = infer_poly(inner, input, ctx);
            ctx.add_constraint(inner_ty, Type::Object(ObjectShape::open_any()));
            Type::Array(Box::new(Type::String))
        }
        Expr::Builtin(Builtin::TypeName, inner) => {
            let _inner_ty = infer_poly(inner, input, ctx);
            Type::String
        }
        Expr::Builtin(Builtin::Map, inner) => {
            let elem_ty = ctx.fresh_var("M");
            ctx.add_constraint(input, Type::Array(Box::new(elem_ty.clone())));
            let out_elem = infer_poly(inner, elem_ty, ctx);
            Type::Array(Box::new(out_elem))
        }
        Expr::Builtin(Builtin::Select, inner) => {
            let _pred_ty = infer_poly(inner, input.clone(), ctx);
            input
        }
        Expr::Builtin(Builtin::Has, inner) => {
            let _key_ty = infer_poly(inner, input.clone(), ctx);
            ctx.add_constraint(
                input,
                Type::union(vec![
                    Type::Object(ObjectShape::open_any()),
                    Type::Array(Box::new(Type::Any)),
                ]),
            );
            Type::Bool
        }
        Expr::Builtin(Builtin::Contains, inner) => {
            let _arg_ty = infer_poly(inner, input, ctx);
            Type::Bool
        }
        Expr::Builtin(Builtin::StartsWith, inner) | Expr::Builtin(Builtin::EndsWith, inner) => {
            let arg_ty = infer_poly(inner, input.clone(), ctx);
            ctx.add_constraint(input, Type::String);
            ctx.add_constraint(arg_ty, Type::String);
            Type::Bool
        }
        Expr::Builtin(Builtin::Reverse, inner) => {
            let inner_ty = infer_poly(inner, input, ctx);
            let elem = ctx.fresh_var("R");
            ctx.add_constraint(inner_ty, Type::Array(Box::new(elem.clone())));
            Type::Array(Box::new(elem))
        }
        Expr::Builtin(Builtin::Sort, inner) => {
            let inner_ty = infer_poly(inner, input, ctx);
            let elem = ctx.fresh_var("S");
            ctx.add_constraint(inner_ty, Type::Array(Box::new(elem.clone())));
            Type::Array(Box::new(elem))
        }
        Expr::Builtin(Builtin::ToString, inner) => {
            let _inner_ty = infer_poly(inner, input, ctx);
            Type::String
        }
        Expr::Builtin(Builtin::ToNumber, inner) => {
            let inner_ty = infer_poly(inner, input, ctx);
            ctx.add_constraint(inner_ty, Type::union(vec![Type::Number, Type::String]));
            Type::Number
        }
        Expr::Builtin(Builtin::Abs, inner)
        | Expr::Builtin(Builtin::Floor, inner)
        | Expr::Builtin(Builtin::Ceil, inner) => {
            let inner_ty = infer_poly(inner, input, ctx);
            ctx.add_constraint(inner_ty, Type::Number);
            Type::Number
        }
        Expr::Builtin(Builtin::Add, inner) => {
            let inner_ty = infer_poly(inner, input, ctx);
            let elem = ctx.fresh_var("A");
            ctx.add_constraint(inner_ty, Type::Array(Box::new(elem.clone())));
            Type::Any
        }
        Expr::Builtin(Builtin::Min, inner) | Expr::Builtin(Builtin::Max, inner) => {
            let inner_ty = infer_poly(inner, input, ctx);
            let elem = ctx.fresh_var("X");
            ctx.add_constraint(inner_ty, Type::NonEmptyArray(Box::new(elem.clone())));
            elem
        }
        Expr::Builtin(Builtin::Split, inner) => {
            let delim_ty = infer_poly(inner, input.clone(), ctx);
            ctx.add_constraint(input, Type::String);
            ctx.add_constraint(delim_ty, Type::String);
            Type::Array(Box::new(Type::String))
        }
        Expr::Builtin(Builtin::Join, inner) => {
            let sep_ty = infer_poly(inner, input.clone(), ctx);
            ctx.add_constraint(
                input,
                Type::Array(Box::new(Type::union(vec![
                    Type::String,
                    Type::Number,
                    Type::Bool,
                    Type::Null,
                ]))),
            );
            ctx.add_constraint(sep_ty, Type::String);
            Type::String
        }
        Expr::Call(_, inner) => {
            let _arg_ty = infer_poly(inner, input, ctx);
            Type::Any
        }
        Expr::Field(inner, name) => {
            let inner_ty = infer_poly(inner, input, ctx);
            let field_ty = ctx.fresh_var("F");
            let row = ctx.fresh_row_var("R");
            ctx.add_constraint(
                inner_ty,
                Type::Object(ObjectShape::with_required_field_row(
                    name.clone(),
                    field_ty.clone(),
                    row,
                )),
            );
            field_ty
        }
        Expr::Index(inner, idx) => {
            let inner_ty = infer_poly(inner, input, ctx);
            let elem_ty = ctx.fresh_var("I");

            if *idx == 0 {
                ctx.add_constraint(inner_ty, Type::NonEmptyArray(Box::new(elem_ty.clone())));
                elem_ty
            } else {
                let mut tuple_items = Vec::new();
                for i in 0..=*idx {
                    if i == *idx {
                        tuple_items.push(elem_ty.clone());
                    } else {
                        tuple_items.push(ctx.fresh_var("P"));
                    }
                }

                ctx.add_constraint(
                    inner_ty,
                    Type::union(vec![
                        Type::Array(Box::new(elem_ty.clone())),
                        Type::Tuple(tuple_items),
                    ]),
                );

                Type::union(vec![elem_ty, Type::Null])
            }
        }
        Expr::Lookup(inner, key) => {
            let inner_ty = infer_poly(inner, input.clone(), ctx);
            let key_ty = infer_poly(key, input, ctx);
            let elem_ty = ctx.fresh_var("L");
            ctx.add_constraint(key_ty, Type::union(vec![Type::String, Type::Number]));
            ctx.add_constraint(
                inner_ty,
                Type::union(vec![
                    Type::Array(Box::new(elem_ty.clone())),
                    Type::Object(ObjectShape::open_any()),
                ]),
            );
            Type::union(vec![elem_ty, Type::Null])
        }
        Expr::Optional(_inner) => Type::union(vec![Type::Any, Type::Null]),
        Expr::TryCatch {
            try_expr,
            catch_expr,
        } => {
            let try_ty = infer_poly(try_expr, input.clone(), ctx);
            let catch_ty = infer_poly(catch_expr, input, ctx);
            Type::union(vec![try_ty, catch_ty])
        }
        Expr::IfElse {
            cond,
            then_branch,
            else_branch,
        } => {
            let _cond_ty = infer_poly(cond, input.clone(), ctx);
            let then_ty = infer_poly(then_branch, input.clone(), ctx);
            let else_ty = infer_poly(else_branch, input, ctx);
            Type::union(vec![then_ty, else_ty])
        }
        Expr::ArrayLiteral(items) => Type::Tuple(
            items
                .iter()
                .map(|item| infer_poly(item, input.clone(), ctx))
                .collect(),
        ),
        Expr::ObjectLiteral(fields) => {
            let mut out = BTreeMap::new();
            for (k, v) in fields {
                out.insert(k.clone(), infer_poly(v, input.clone(), ctx));
            }
            Type::Object(ObjectShape::closed(out))
        }
    }
}

fn infer_poly_binary(op: BinaryOp, left: Type, right: Type, ctx: &mut PolyInferCtx) -> Type {
    match op {
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
            ctx.add_constraint(left, Type::Number);
            ctx.add_constraint(right, Type::Number);
            Type::Number
        }
        BinaryOp::Eq | BinaryOp::Ne => Type::Bool,
        BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => Type::Bool,
        BinaryOp::And | BinaryOp::Or => Type::Bool,
        BinaryOp::Alt => Type::union(vec![left, right]),
    }
}

#[derive(Default)]
struct PolyInferCtx {
    next_var_id: usize,
    next_row_id: usize,
    constraints: Vec<(Type, Type)>,
    bounds: HashMap<String, Type>,
}

impl PolyInferCtx {
    fn fresh_var(&mut self, prefix: &str) -> Type {
        let id = self.next_var_id;
        self.next_var_id += 1;
        let name = format!("{prefix}{id}");
        self.bounds.entry(name.clone()).or_insert(Type::Any);
        Type::Generic(name)
    }

    fn fresh_row_var(&mut self, prefix: &str) -> String {
        let id = self.next_row_id;
        self.next_row_id += 1;
        format!("{prefix}{id}")
    }

    fn add_constraint(&mut self, lhs: Type, rhs: Type) {
        self.constraints.push((lhs, rhs));
    }

    fn solve(&mut self) {
        for _ in 0..128 {
            let mut changed = false;
            let constraints = self.constraints.clone();
            for (lhs, rhs) in constraints {
                changed |= self.apply_constraint(&lhs, &rhs);
            }
            if !changed {
                break;
            }
        }
    }

    fn apply_constraint(&mut self, lhs: &Type, rhs: &Type) -> bool {
        match lhs {
            Type::Generic(name) => {
                let rhs_resolved = self.resolve_type(rhs);
                let current = self.bounds.get(name).cloned().unwrap_or(Type::Any);
                let next = current.intersect(&rhs_resolved).normalize();
                if next != current {
                    self.bounds.insert(name.clone(), next);
                    true
                } else {
                    false
                }
            }
            Type::Union(items) => {
                let mut changed = false;
                for item in items {
                    changed |= self.apply_constraint(item, rhs);
                }
                changed
            }
            _ => false,
        }
    }

    fn resolve_type(&self, ty: &Type) -> Type {
        let mut visiting = HashSet::new();
        self.resolve_type_inner(ty, &mut visiting)
    }

    fn resolve_type_inner(&self, ty: &Type, visiting: &mut HashSet<String>) -> Type {
        match ty {
            Type::Generic(name) => {
                if visiting.contains(name) {
                    return Type::Generic(name.clone());
                }

                let bound = self.bounds.get(name).cloned().unwrap_or(Type::Any);
                if matches!(bound, Type::Any) {
                    Type::Generic(name.clone())
                } else {
                    visiting.insert(name.clone());
                    let resolved = self.resolve_type_inner(&bound, visiting);
                    visiting.remove(name);
                    resolved
                }
            }
            Type::Array(inner) => Type::Array(Box::new(self.resolve_type_inner(inner, visiting))),
            Type::NonEmptyArray(inner) => {
                Type::NonEmptyArray(Box::new(self.resolve_type_inner(inner, visiting)))
            }
            Type::Tuple(items) => Type::Tuple(
                items
                    .iter()
                    .map(|item| self.resolve_type_inner(item, visiting))
                    .collect(),
            ),
            Type::Object(shape) => {
                let fields = shape
                    .fields
                    .iter()
                    .map(|(k, v)| (k.clone(), self.resolve_type_inner(v, visiting)))
                    .collect();
                let tail = match &shape.tail {
                    RowTail::Closed => RowTail::Closed,
                    RowTail::Open(rest) => {
                        RowTail::Open(Box::new(self.resolve_type_inner(rest, visiting)))
                    }
                    RowTail::Var(name) => RowTail::Var(name.clone()),
                };
                Type::Object(ObjectShape { fields, tail })
            }
            Type::Union(items) => Type::union(
                items
                    .iter()
                    .map(|item| self.resolve_type_inner(item, visiting)),
            ),
            other => other.clone(),
        }
    }
}

fn field_output_type(input: &Type, key: &str) -> Type {
    match input {
        Type::Object(shape) => {
            if let Some(value_ty) = shape.fields.get(key) {
                value_ty.clone()
            } else {
                match &shape.tail {
                    RowTail::Closed => Type::Null,
                    RowTail::Open(rest) => Type::union(vec![rest.as_ref().clone(), Type::Null]),
                    RowTail::Var(_) => Type::union(vec![Type::Any, Type::Null]),
                }
            }
        }
        Type::Union(items) => Type::union(items.iter().map(|item| field_output_type(item, key))),
        Type::Any => Type::Any,
        _ => Type::Never,
    }
}

fn index_output_type(input: &Type, index: usize) -> Type {
    match input {
        Type::Array(item) => Type::union(vec![item.as_ref().clone(), Type::Null]),
        Type::NonEmptyArray(item) => {
            if index == 0 {
                item.as_ref().clone()
            } else {
                Type::union(vec![item.as_ref().clone(), Type::Null])
            }
        }
        Type::Tuple(items) => items.get(index).cloned().unwrap_or(Type::Null),
        Type::Union(items) => Type::union(items.iter().map(|item| index_output_type(item, index))),
        Type::Any => Type::Any,
        _ => Type::Never,
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum StaticLookupKey {
    String(String),
    Index(usize),
}

fn static_lookup_key(key_expr: &Expr) -> Option<StaticLookupKey> {
    let Expr::Literal(value) = key_expr else {
        return None;
    };

    match value {
        Value::String(s) => Some(StaticLookupKey::String(s.clone())),
        Value::Number(n) => {
            if let Some(idx) = n.as_u64().and_then(|u| usize::try_from(u).ok()) {
                return Some(StaticLookupKey::Index(idx));
            }

            let float = n.as_f64()?;
            if float.is_finite() && float >= 0.0 && float.fract() == 0.0 {
                let idx = float as u64;
                usize::try_from(idx).ok().map(StaticLookupKey::Index)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn lookup_output_type(input: &Type, key_ty: &Type, static_key: Option<StaticLookupKey>) -> Type {
    match input {
        Type::Array(elem) => {
            if key_ty.intersect(&Type::Number).normalize().is_never() {
                Type::Never
            } else {
                Type::union(vec![elem.as_ref().clone(), Type::Null])
            }
        }
        Type::NonEmptyArray(elem) => {
            if key_ty.intersect(&Type::Number).normalize().is_never() {
                Type::Never
            } else if matches!(static_key, Some(StaticLookupKey::Index(0))) {
                elem.as_ref().clone()
            } else {
                Type::union(vec![elem.as_ref().clone(), Type::Null])
            }
        }
        Type::Tuple(items) => {
            if key_ty.intersect(&Type::Number).normalize().is_never() {
                Type::Never
            } else if let Some(StaticLookupKey::Index(i)) = static_key {
                items.get(i).cloned().unwrap_or(Type::Null)
            } else {
                let mut out = items.clone();
                out.push(Type::Null);
                Type::union(out)
            }
        }
        Type::Object(shape) => {
            if key_ty.intersect(&Type::String).normalize().is_never() {
                Type::Never
            } else {
                object_lookup_output(shape, static_key)
            }
        }
        Type::Union(items) => Type::union(
            items
                .iter()
                .map(|item| lookup_output_type(item, key_ty, static_key.clone())),
        ),
        Type::Any => Type::Any,
        _ => Type::Never,
    }
}

fn object_lookup_output(shape: &ObjectShape, static_key: Option<StaticLookupKey>) -> Type {
    if let Some(StaticLookupKey::String(key)) = static_key {
        if let Some(value_ty) = shape.fields.get(&key) {
            return value_ty.clone();
        }

        return match &shape.tail {
            RowTail::Closed => Type::Null,
            RowTail::Open(rest) => Type::union(vec![rest.as_ref().clone(), Type::Null]),
            RowTail::Var(_) => Type::union(vec![Type::Any, Type::Null]),
        };
    }

    let mut out: Vec<Type> = shape.fields.values().cloned().collect();
    match &shape.tail {
        RowTail::Closed => {}
        RowTail::Open(rest) => out.push(rest.as_ref().clone()),
        RowTail::Var(_) => out.push(Type::Any),
    }
    out.push(Type::Null);
    Type::union(out)
}

fn unify_pattern(pattern: &Type, actual: &Type, bindings: &mut HashMap<String, Type>) -> bool {
    match pattern {
        Type::Any => true,
        Type::Never => matches!(actual, Type::Never),
        Type::Generic(name) => {
            if let Some(bound) = bindings.get(name) {
                actual.is_subtype_of(bound) && bound.is_subtype_of(actual)
            } else {
                bindings.insert(name.clone(), actual.clone());
                true
            }
        }
        Type::Union(options) => options.iter().any(|option| {
            let mut snapshot = bindings.clone();
            let ok = unify_pattern(option, actual, &mut snapshot);
            if ok {
                *bindings = snapshot;
            }
            ok
        }),
        Type::Array(pattern_item) => match actual {
            Type::Array(actual_item) | Type::NonEmptyArray(actual_item) => {
                unify_pattern(pattern_item, actual_item, bindings)
            }
            Type::Tuple(items) => items
                .iter()
                .all(|item| unify_pattern(pattern_item, item, bindings)),
            _ => actual.is_subtype_of(pattern),
        },
        Type::NonEmptyArray(pattern_item) => match actual {
            Type::NonEmptyArray(actual_item) => unify_pattern(pattern_item, actual_item, bindings),
            Type::Tuple(items) => {
                !items.is_empty()
                    && items
                        .iter()
                        .all(|item| unify_pattern(pattern_item, item, bindings))
            }
            _ => false,
        },
        Type::Tuple(pattern_items) => match actual {
            Type::Tuple(actual_items) => {
                pattern_items.len() == actual_items.len()
                    && pattern_items
                        .iter()
                        .zip(actual_items.iter())
                        .all(|(p, a)| unify_pattern(p, a, bindings))
            }
            _ => false,
        },
        Type::Object(pattern_shape) => match actual {
            Type::Object(actual_shape) => {
                for (key, pattern_ty) in &pattern_shape.fields {
                    let Some(actual_ty) = actual_shape.fields.get(key) else {
                        return false;
                    };

                    if !unify_pattern(pattern_ty, actual_ty, bindings) {
                        return false;
                    }
                }

                for (key, actual_ty) in &actual_shape.fields {
                    if pattern_shape.fields.contains_key(key) {
                        continue;
                    }

                    match &pattern_shape.tail {
                        RowTail::Closed => return false,
                        RowTail::Open(rest) => {
                            if !unify_pattern(rest, actual_ty, bindings) {
                                return false;
                            }
                        }
                        RowTail::Var(_) => {}
                    }
                }

                match (&pattern_shape.tail, &actual_shape.tail) {
                    (RowTail::Closed, RowTail::Closed) => true,
                    (RowTail::Closed, _) => false,
                    (RowTail::Open(_), RowTail::Var(_)) => true,
                    (RowTail::Open(rest), RowTail::Open(actual_rest)) => {
                        unify_pattern(rest, actual_rest, bindings)
                    }
                    (RowTail::Open(_), RowTail::Closed) => true,
                    (RowTail::Var(_), _) => true,
                }
            }
            _ => false,
        },
        _ => pattern == actual || actual.is_subtype_of(pattern),
    }
}

fn substitute_generics(ty: &Type, bindings: &HashMap<String, Type>) -> Type {
    match ty {
        Type::Generic(name) => bindings.get(name).cloned().unwrap_or(Type::Any),
        Type::Array(inner) => Type::Array(Box::new(substitute_generics(inner, bindings))),
        Type::NonEmptyArray(inner) => {
            Type::NonEmptyArray(Box::new(substitute_generics(inner, bindings)))
        }
        Type::Tuple(items) => Type::Tuple(
            items
                .iter()
                .map(|item| substitute_generics(item, bindings))
                .collect(),
        ),
        Type::Object(shape) => {
            let fields = shape
                .fields
                .iter()
                .map(|(k, v)| (k.clone(), substitute_generics(v, bindings)))
                .collect();
            let tail = match &shape.tail {
                RowTail::Closed => RowTail::Closed,
                RowTail::Open(rest) => RowTail::Open(Box::new(substitute_generics(rest, bindings))),
                RowTail::Var(name) => RowTail::Var(name.clone()),
            };
            Type::Object(ObjectShape { fields, tail })
        }
        Type::Union(items) => {
            Type::union(items.iter().map(|item| substitute_generics(item, bindings)))
        }
        other => other.clone(),
    }
}

fn collect_free_generics(ty: &Type, into: &mut BTreeSet<String>) {
    match ty {
        Type::Generic(name) => {
            into.insert(name.clone());
        }
        Type::Array(inner) | Type::NonEmptyArray(inner) => collect_free_generics(inner, into),
        Type::Tuple(items) => {
            for item in items {
                collect_free_generics(item, into);
            }
        }
        Type::Object(shape) => {
            for field_ty in shape.fields.values() {
                collect_free_generics(field_ty, into);
            }
            match &shape.tail {
                RowTail::Closed => {}
                RowTail::Open(rest) => collect_free_generics(rest, into),
                RowTail::Var(name) => {
                    into.insert(name.clone());
                }
            }
        }
        Type::Union(items) => {
            for item in items {
                collect_free_generics(item, into);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::{Builtin, infer_expr_scheme, infer_expr_type, infer_predicate_refinement};
    use crate::minijq::ast::Expr;
    use crate::minijq::types::{ObjectShape, RowTail, Type};

    #[test]
    fn first_instantiates_generic() {
        let scheme = Builtin::First.scheme();
        let out = scheme.instantiate_output(&Type::NonEmptyArray(Box::new(Type::Number)));
        assert_eq!(out, Some(Type::Number));
    }

    #[test]
    fn infer_first_then_length() {
        let expr = Expr::pipe(
            Expr::builtin(Builtin::First, Expr::identity()),
            Expr::builtin(Builtin::Length, Expr::identity()),
        );
        let inferred = infer_expr_type(
            &expr,
            &Type::NonEmptyArray(Box::new(Type::union(vec![Type::String, Type::Number]))),
        );
        assert_eq!(inferred, Type::Number);
    }

    #[test]
    fn infer_comparison_returns_bool() {
        let expr = Expr::gt(Expr::identity(), Expr::literal(serde_json::json!(10)));
        assert_eq!(infer_expr_type(&expr, &Type::Number), Type::Bool);
    }

    #[test]
    fn infer_isnumber_comparison_partitions() {
        let expr = Expr::and(
            Expr::gt(Expr::identity(), Expr::literal(serde_json::json!(true))),
            Expr::lt(Expr::identity(), Expr::literal(serde_json::json!(""))),
        );

        assert_eq!(
            infer_expr_type(&expr, &Type::Number),
            Type::BoolLiteral(true)
        );
        assert_eq!(
            infer_expr_type(&expr, &Type::Bool),
            Type::BoolLiteral(false)
        );
        assert_eq!(
            infer_expr_type(&expr, &Type::String),
            Type::BoolLiteral(false)
        );
        assert_eq!(
            infer_expr_type(&expr, &Type::Array(Box::new(Type::Any))),
            Type::BoolLiteral(false)
        );
        assert_eq!(
            infer_expr_type(&expr, &Type::Object(ObjectShape::open_any())),
            Type::BoolLiteral(false)
        );
    }

    #[test]
    fn infer_predicate_refinement_for_isnumber() {
        let expr = Expr::and(
            Expr::gt(Expr::identity(), Expr::literal(serde_json::json!(true))),
            Expr::lt(Expr::identity(), Expr::literal(serde_json::json!(""))),
        );

        let refinement = infer_predicate_refinement(&expr, &Type::Any).expect("bool predicate");

        assert_eq!(refinement.true_input, Type::Number);
        assert_eq!(
            refinement.false_input,
            Type::union(vec![
                Type::Null,
                Type::Bool,
                Type::String,
                Type::Array(Box::new(Type::Any)),
                Type::Object(ObjectShape::open_any()),
            ])
        );
        assert!(refinement.unknown_input.is_never());
        assert!(refinement.is_total());
    }

    #[test]
    fn infer_predicate_refinement_for_isboolean() {
        let expr = Expr::or(
            Expr::eq(Expr::identity(), Expr::literal(serde_json::json!(true))),
            Expr::eq(Expr::identity(), Expr::literal(serde_json::json!(false))),
        );

        let refinement = infer_predicate_refinement(&expr, &Type::Any).expect("bool predicate");

        assert_eq!(refinement.true_input, Type::Bool);
        assert_eq!(
            refinement.false_input,
            Type::union(vec![
                Type::Null,
                Type::Number,
                Type::String,
                Type::Array(Box::new(Type::Any)),
                Type::Object(ObjectShape::open_any()),
            ])
        );
        assert!(refinement.unknown_input.is_never());
        assert!(refinement.is_total());
    }

    #[test]
    fn infer_literal_boolean_results() {
        let eq = Expr::eq(
            Expr::literal(serde_json::json!(true)),
            Expr::literal(serde_json::json!(false)),
        );
        let not = Expr::not(Expr::literal(serde_json::json!(true)));

        assert_eq!(infer_expr_type(&eq, &Type::Any), Type::BoolLiteral(false));
        assert_eq!(infer_expr_type(&not, &Type::Any), Type::BoolLiteral(false));
    }

    #[test]
    fn infer_if_else_unions_outputs() {
        let expr = Expr::if_else(
            Expr::literal(serde_json::json!(true)),
            Expr::literal(serde_json::json!(1)),
            Expr::literal(serde_json::json!("x")),
        );
        assert_eq!(
            infer_expr_type(&expr, &Type::Any),
            Type::union(vec![
                Type::NumberLiteral("1".to_string()),
                Type::StringLiteral("x".to_string())
            ])
        );
    }

    #[test]
    fn infer_map_over_number_array() {
        let expr = Expr::builtin(
            Builtin::Map,
            Expr::add(Expr::identity(), Expr::literal(serde_json::json!(1))),
        );
        let out = infer_expr_type(&expr, &Type::Array(Box::new(Type::Number)));
        assert_eq!(out, Type::Array(Box::new(Type::Number)));
    }

    #[test]
    fn infer_select_preserves_input_domain_without_null() {
        let expr = Expr::builtin(
            Builtin::Select,
            Expr::gt(Expr::identity(), Expr::literal(serde_json::json!(0))),
        );
        let out = infer_expr_type(&expr, &Type::Number);
        assert_eq!(out, Type::Number);
    }

    #[test]
    fn infer_select_isboolean_refines_to_bool() {
        let isboolean = Expr::or(
            Expr::eq(Expr::identity(), Expr::literal(serde_json::json!(true))),
            Expr::eq(Expr::identity(), Expr::literal(serde_json::json!(false))),
        );
        let expr = Expr::builtin(Builtin::Select, isboolean);
        let out = infer_expr_type(&expr, &Type::Any);
        assert_eq!(out, Type::Bool);
    }

    #[test]
    fn infer_select_isarray_refines_to_array() {
        let isarray = Expr::and(
            Expr::gte(Expr::identity(), Expr::literal(serde_json::json!([]))),
            Expr::lt(Expr::identity(), Expr::literal(serde_json::json!({}))),
        );
        let expr = Expr::builtin(Builtin::Select, isarray);
        let out = infer_expr_type(&expr, &Type::Any);
        assert_eq!(out, Type::Array(Box::new(Type::Any)));
    }

    #[test]
    fn infer_has_requires_indexable_input() {
        let expr = Expr::builtin(Builtin::Has, Expr::literal(serde_json::json!("x")));
        let object_out = infer_expr_type(
            &expr,
            &Type::Object(ObjectShape::with_required_field("x", Type::Number)),
        );
        assert_eq!(object_out, Type::Bool);

        let number_out = infer_expr_type(&expr, &Type::Number);
        assert_eq!(number_out, Type::Never);
    }

    #[test]
    fn infer_contains_is_bool_for_well_typed_input() {
        let expr = Expr::builtin(Builtin::Contains, Expr::literal(serde_json::json!([1])));
        let out = infer_expr_type(&expr, &Type::Array(Box::new(Type::Number)));
        assert_eq!(out, Type::Bool);
    }

    #[test]
    fn infer_startswith_requires_string() {
        let expr = Expr::builtin(Builtin::StartsWith, Expr::literal(serde_json::json!("ab")));
        assert_eq!(infer_expr_type(&expr, &Type::String), Type::Bool);
        assert_eq!(infer_expr_type(&expr, &Type::Number), Type::Never);
    }

    #[test]
    fn infer_reverse_and_sort_on_tuples() {
        let reverse = Expr::builtin(Builtin::Reverse, Expr::identity());
        let sort = Expr::builtin(Builtin::Sort, Expr::identity());
        let input = Type::Tuple(vec![Type::Number, Type::String, Type::Bool]);

        assert_eq!(
            infer_expr_type(&reverse, &input),
            Type::Tuple(vec![Type::Bool, Type::String, Type::Number])
        );
        assert_eq!(
            infer_expr_type(&sort, &input),
            Type::Array(Box::new(Type::union(vec![
                Type::Bool,
                Type::Number,
                Type::String
            ])))
        );
    }

    #[test]
    fn infer_lookup_with_string_key_on_object() {
        let expr = Expr::lookup(Expr::identity(), Expr::literal(serde_json::json!("name")));
        let input = Type::Object(ObjectShape::with_required_field("name", Type::String));
        assert_eq!(infer_expr_type(&expr, &input), Type::String);
    }

    #[test]
    fn infer_lookup_with_number_key_on_tuple() {
        let expr = Expr::lookup(Expr::identity(), Expr::literal(serde_json::json!(1)));
        let input = Type::Tuple(vec![Type::Number, Type::String]);
        assert_eq!(infer_expr_type(&expr, &input), Type::String);
    }

    #[test]
    fn infer_optional_wraps_with_null() {
        let expr = Expr::optional(Expr::field(Expr::identity(), "x"));
        let out = infer_expr_type(&expr, &Type::Number);
        assert_eq!(out, Type::Null);
    }

    #[test]
    fn infer_conversion_and_numeric_builtins() {
        let tostring = Expr::builtin(Builtin::ToString, Expr::identity());
        let tonumber = Expr::builtin(Builtin::ToNumber, Expr::identity());
        let abs = Expr::builtin(Builtin::Abs, Expr::identity());

        assert_eq!(infer_expr_type(&tostring, &Type::Bool), Type::String);
        assert_eq!(infer_expr_type(&tonumber, &Type::String), Type::Number);
        assert_eq!(infer_expr_type(&tonumber, &Type::Bool), Type::Never);
        assert_eq!(infer_expr_type(&abs, &Type::Number), Type::Number);
    }

    #[test]
    fn infer_add_min_max_types() {
        let add = Expr::builtin(Builtin::Add, Expr::identity());
        let min = Expr::builtin(Builtin::Min, Expr::identity());
        let max = Expr::builtin(Builtin::Max, Expr::identity());

        assert_eq!(
            infer_expr_type(&add, &Type::Array(Box::new(Type::Number))),
            Type::Number
        );
        assert_eq!(
            infer_expr_type(&add, &Type::Array(Box::new(Type::String))),
            Type::String
        );
        assert_eq!(
            infer_expr_type(&min, &Type::NonEmptyArray(Box::new(Type::Bool))),
            Type::Bool
        );
        assert_eq!(
            infer_expr_type(&max, &Type::Tuple(vec![Type::Number, Type::String])),
            Type::union(vec![Type::Number, Type::String])
        );
    }

    #[test]
    fn infer_split_and_join_types() {
        let split = Expr::builtin(Builtin::Split, Expr::literal(serde_json::json!(",")));
        let join = Expr::builtin(Builtin::Join, Expr::literal(serde_json::json!("-")));

        assert_eq!(
            infer_expr_type(&split, &Type::String),
            Type::Array(Box::new(Type::String))
        );
        assert_eq!(
            infer_expr_type(&join, &Type::Array(Box::new(Type::String))),
            Type::String
        );
        assert_eq!(
            infer_expr_type(
                &join,
                &Type::Array(Box::new(Type::union(vec![
                    Type::Number,
                    Type::Bool,
                    Type::Null,
                ]))),
            ),
            Type::String
        );
        assert_eq!(infer_expr_type(&split, &Type::Number), Type::Never);
    }

    #[test]
    fn infer_try_catch_unions_outputs() {
        let expr = Expr::try_catch(
            Expr::field(Expr::identity(), "x"),
            Expr::literal(serde_json::json!(0)),
        );
        let out = infer_expr_type(&expr, &Type::Object(ObjectShape::open_any()));
        assert_eq!(out, Type::Any);
    }

    #[test]
    fn infer_error_returns_never() {
        let expr = Expr::builtin(Builtin::Error, Expr::identity());
        assert_eq!(infer_expr_type(&expr, &Type::Any), Type::Never);
    }

    #[test]
    fn infer_boolean_guard_else_error_is_number() {
        let expr = Expr::if_else(
            Expr::or(
                Expr::eq(Expr::identity(), Expr::literal(serde_json::json!(true))),
                Expr::eq(Expr::identity(), Expr::literal(serde_json::json!(false))),
            ),
            Expr::literal(serde_json::json!(1)),
            Expr::builtin(Builtin::Error, Expr::identity()),
        );
        assert_eq!(
            infer_expr_type(&expr, &Type::Any),
            Type::NumberLiteral("1".to_string())
        );
    }

    #[test]
    fn infer_scheme_identity_is_generic() {
        let scheme = infer_expr_scheme(&Expr::identity());
        assert_eq!(scheme.input, scheme.output);
        assert_eq!(scheme.vars.len(), 1);
    }

    #[test]
    fn infer_scheme_error_is_any_to_never() {
        let scheme = infer_expr_scheme(&Expr::builtin(Builtin::Error, Expr::identity()));
        assert_eq!(scheme.output, Type::Never);
        assert_eq!(scheme.vars.len(), 1);
        assert_eq!(scheme.input, Type::Generic(scheme.vars[0].clone()));
    }

    #[test]
    fn infer_scheme_add_self_is_number_to_number() {
        let scheme = infer_expr_scheme(&Expr::add(Expr::identity(), Expr::identity()));
        assert_eq!(scheme.input, Type::Number);
        assert_eq!(scheme.output, Type::Number);
        assert!(scheme.vars.is_empty());
    }

    #[test]
    fn infer_scheme_first_is_nonempty_array_to_element() {
        let scheme = infer_expr_scheme(&Expr::builtin(Builtin::First, Expr::identity()));
        match (&scheme.input, &scheme.output) {
            (Type::NonEmptyArray(inner), Type::Generic(name)) => {
                assert_eq!(inner.as_ref(), &Type::Generic(name.clone()));
            }
            _ => panic!("unexpected scheme: {scheme:?}"),
        }
    }

    #[test]
    fn infer_scheme_field_contains_row_variable() {
        let scheme = infer_expr_scheme(&Expr::field(Expr::identity(), "x"));
        match scheme.input {
            Type::Object(ObjectShape {
                fields,
                tail: RowTail::Var(_),
            }) => {
                assert!(fields.contains_key("x"));
            }
            other => panic!("expected object with row variable, got {other:?}"),
        }
    }

    #[test]
    fn infer_scheme_nested_object_sum() {
        let expr = Expr::add(
            Expr::field(Expr::field(Expr::identity(), "metrics"), "a"),
            Expr::field(Expr::field(Expr::identity(), "metrics"), "b"),
        );

        let scheme = infer_expr_scheme(&expr);
        assert_eq!(scheme.output, Type::Number);

        match scheme.input {
            Type::Object(root) => {
                let metrics = root
                    .fields
                    .get("metrics")
                    .expect("metrics field must be required");
                match metrics {
                    Type::Object(inner) => {
                        assert!(inner.fields.contains_key("a"));
                        assert!(inner.fields.contains_key("b"));
                        assert_eq!(inner.fields.get("a"), Some(&Type::Number));
                        assert_eq!(inner.fields.get("b"), Some(&Type::Number));
                    }
                    other => panic!("expected nested object for metrics, got {other:?}"),
                }
            }
            other => panic!("expected object input, got {other:?}"),
        }
    }
}
