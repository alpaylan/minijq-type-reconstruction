use crate::minijq::eval::RuntimeTypeError;
use crate::minijq::types::{ObjectShape, RowTail, Type};
use serde_json::Value;
use std::cmp::Reverse;
use std::collections::BTreeMap;

const MAX_ISSUES: usize = 16;
const MAX_PREVIEW_ARRAY_ITEMS: usize = 16;
const MAX_PREVIEW_OBJECT_FIELDS: usize = 32;
const MAX_PREVIEW_STRING_LITERAL_LEN: usize = 64;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValidationIssueKind {
    TypeMismatch {
        expected: Type,
        actual: Type,
    },
    MissingField {
        field: String,
        expected: Type,
    },
    UnexpectedField {
        field: String,
    },
    LengthMismatch {
        expected_len: usize,
        actual_len: usize,
    },
    NonEmptyRequired,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ValidationIssue {
    pub path: String,
    pub kind: ValidationIssueKind,
}

impl ValidationIssue {
    pub fn message(&self) -> String {
        match &self.kind {
            ValidationIssueKind::TypeMismatch { expected, actual } => {
                format!("{}: expected {}, got {}", self.path, expected, actual)
            }
            ValidationIssueKind::MissingField { field, expected } => {
                format!(
                    "{}: missing required field `{field}` (expected {})",
                    self.path, expected
                )
            }
            ValidationIssueKind::UnexpectedField { field } => {
                format!("{}: unexpected field `{field}`", self.path)
            }
            ValidationIssueKind::LengthMismatch {
                expected_len,
                actual_len,
            } => {
                format!(
                    "{}: tuple length mismatch (expected {}, got {})",
                    self.path, expected_len, actual_len
                )
            }
            ValidationIssueKind::NonEmptyRequired => {
                format!("{}: expected a non-empty array", self.path)
            }
        }
    }

    fn score(&self) -> (usize, usize) {
        let depth = self.path.as_bytes().iter().filter(|c| **c == b'[').count();
        let specificity = match self.kind {
            ValidationIssueKind::MissingField { .. } => 3,
            ValidationIssueKind::TypeMismatch { .. } => 2,
            ValidationIssueKind::LengthMismatch { .. } => 2,
            ValidationIssueKind::NonEmptyRequired => 2,
            ValidationIssueKind::UnexpectedField { .. } => 1,
        };
        (depth, specificity)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SchemeValidationReport {
    pub reconstructed_input_type: Type,
    pub actual_input_type: Type,
    pub accepted_by_reconstruction: bool,
    pub subset_hits: Vec<Type>,
    pub issues: Vec<ValidationIssue>,
    pub runtime_error: Option<RuntimeTypeError>,
}

impl SchemeValidationReport {
    pub fn pretty(&self) -> String {
        let mut out = String::new();
        out.push_str(&format!(
            "Reconstructed input domain: {}\n",
            self.reconstructed_input_type
        ));
        out.push_str(&format!("Actual input type: {}\n", self.actual_input_type));
        out.push_str(&format!(
            "Accepted by reconstructed domain: {}\n",
            self.accepted_by_reconstruction
        ));

        if !self.subset_hits.is_empty() {
            let markers = self
                .subset_hits
                .iter()
                .map(|ty| format!("Subset<{ty}>"))
                .collect::<Vec<_>>()
                .join(", ");
            out.push_str(&format!("Subset markers hit: {markers}\n"));
        }

        if let Some(err) = &self.runtime_error {
            out.push_str(&format!("Runtime error: {err}\n"));
        }

        if self.issues.is_empty() {
            out.push_str("Validation issues: none\n");
        } else {
            out.push_str("Validation issues:\n");
            for issue in &self.issues {
                out.push_str(&format!("  - {}\n", issue.message()));
            }
        }

        out
    }
}

pub fn validate_input_against_reconstruction(
    input: &Value,
    reconstructed_input_type: &Type,
    subset_types: &[Type],
    runtime_error: Option<&RuntimeTypeError>,
) -> SchemeValidationReport {
    let actual_input_type = preview_type(input).normalize();
    let mut issues = validate_value_against_type(input, reconstructed_input_type);
    issues.sort_by_key(|issue| {
        let (depth, specificity) = issue.score();
        (Reverse(depth), Reverse(specificity), issue.path.clone())
    });
    if issues.len() > MAX_ISSUES {
        issues.truncate(MAX_ISSUES);
    }

    let subset_hits = subset_types
        .iter()
        .map(|ty| ty.clone().normalize())
        .filter(|subset| validate_value_against_type(input, subset).is_empty())
        .collect::<Vec<_>>();

    SchemeValidationReport {
        reconstructed_input_type: reconstructed_input_type.clone().normalize(),
        actual_input_type,
        accepted_by_reconstruction: issues.is_empty(),
        subset_hits,
        issues,
        runtime_error: runtime_error.cloned(),
    }
}

pub fn validate_value_against_type(value: &Value, expected: &Type) -> Vec<ValidationIssue> {
    let mut issues = Vec::new();
    validate_value_against_type_at(value, expected, "$".to_string(), &mut issues);
    issues
}

fn validate_value_against_type_at(
    value: &Value,
    expected: &Type,
    path: String,
    issues: &mut Vec<ValidationIssue>,
) {
    if issues.len() >= MAX_ISSUES {
        return;
    }

    let expected = expected.clone().normalize();

    match expected {
        Type::Any | Type::Generic(_) => {}
        Type::Never => push_mismatch(path, Type::Never, value, issues),
        Type::Null => {
            if !matches!(value, Value::Null) {
                push_mismatch(path, Type::Null, value, issues);
            }
        }
        Type::Bool => {
            if !matches!(value, Value::Bool(_)) {
                push_mismatch(path, Type::Bool, value, issues);
            }
        }
        Type::BoolLiteral(expected_bool) => match value {
            Value::Bool(actual_bool) if *actual_bool == expected_bool => {}
            _ => push_mismatch(path, Type::BoolLiteral(expected_bool), value, issues),
        },
        Type::Number => {
            if !matches!(value, Value::Number(_)) {
                push_mismatch(path, Type::Number, value, issues);
            }
        }
        Type::NumberLiteral(expected_number) => match value {
            Value::Number(actual_number) if actual_number.to_string() == expected_number => {}
            _ => push_mismatch(path, Type::NumberLiteral(expected_number), value, issues),
        },
        Type::String => {
            if !matches!(value, Value::String(_)) {
                push_mismatch(path, Type::String, value, issues);
            }
        }
        Type::StringLiteral(expected_string) => match value {
            Value::String(actual_string) if *actual_string == expected_string => {}
            _ => push_mismatch(path, Type::StringLiteral(expected_string), value, issues),
        },
        Type::Union(items) => validate_union(value, items, path, issues),
        Type::Array(elem) => validate_array(value, &elem, path, issues, false),
        Type::NonEmptyArray(elem) => validate_array(value, &elem, path, issues, true),
        Type::Tuple(items) => validate_tuple(value, &items, path, issues),
        Type::Object(shape) => validate_object(value, &shape.fields, &shape.tail, path, issues),
    }
}

fn validate_union(
    value: &Value,
    items: Vec<Type>,
    path: String,
    issues: &mut Vec<ValidationIssue>,
) {
    let mut branch_issues = Vec::new();
    for branch in &items {
        let mut local = Vec::new();
        validate_value_against_type_at(value, branch, path.clone(), &mut local);
        if local.is_empty() {
            return;
        }
        branch_issues.push(local);
    }

    let actual = preview_type(value).normalize();
    if branch_issues
        .iter()
        .all(|local| local.len() == 1 && local[0].path == path)
    {
        issues.push(ValidationIssue {
            path,
            kind: ValidationIssueKind::TypeMismatch {
                expected: Type::union(items),
                actual,
            },
        });
        return;
    }

    if let Some(best) = branch_issues.into_iter().min_by_key(|local| {
        (
            local.len(),
            Reverse(local.iter().map(|issue| issue.score().0).sum::<usize>()),
        )
    }) {
        issues.extend(best);
    }
}

fn validate_array(
    value: &Value,
    elem: &Type,
    path: String,
    issues: &mut Vec<ValidationIssue>,
    require_non_empty: bool,
) {
    let Value::Array(items) = value else {
        issues.push(ValidationIssue {
            path,
            kind: ValidationIssueKind::TypeMismatch {
                expected: if require_non_empty {
                    Type::NonEmptyArray(Box::new(elem.clone()))
                } else {
                    Type::Array(Box::new(elem.clone()))
                },
                actual: preview_type(value),
            },
        });
        return;
    };

    if require_non_empty && items.is_empty() {
        issues.push(ValidationIssue {
            path,
            kind: ValidationIssueKind::NonEmptyRequired,
        });
        return;
    }

    for (idx, item) in items.iter().enumerate() {
        if issues.len() >= MAX_ISSUES {
            break;
        }
        validate_value_against_type_at(item, elem, format!("{path}[{idx}]"), issues);
    }
}

fn validate_tuple(
    value: &Value,
    expected_items: &[Type],
    path: String,
    issues: &mut Vec<ValidationIssue>,
) {
    let Value::Array(items) = value else {
        issues.push(ValidationIssue {
            path,
            kind: ValidationIssueKind::TypeMismatch {
                expected: Type::Tuple(expected_items.to_vec()),
                actual: preview_type(value),
            },
        });
        return;
    };

    if items.len() != expected_items.len() {
        issues.push(ValidationIssue {
            path,
            kind: ValidationIssueKind::LengthMismatch {
                expected_len: expected_items.len(),
                actual_len: items.len(),
            },
        });
        return;
    }

    for (idx, (item, expected)) in items.iter().zip(expected_items.iter()).enumerate() {
        if issues.len() >= MAX_ISSUES {
            break;
        }
        validate_value_against_type_at(item, expected, format!("{path}[{idx}]"), issues);
    }
}

fn validate_object(
    value: &Value,
    expected_fields: &std::collections::BTreeMap<String, Type>,
    tail: &RowTail,
    path: String,
    issues: &mut Vec<ValidationIssue>,
) {
    let Value::Object(map) = value else {
        issues.push(ValidationIssue {
            path,
            kind: ValidationIssueKind::TypeMismatch {
                expected: Type::Object(ObjectShape {
                    fields: expected_fields.clone(),
                    tail: tail.clone(),
                }),
                actual: preview_type(value),
            },
        });
        return;
    };

    for (field, expected_ty) in expected_fields {
        if issues.len() >= MAX_ISSUES {
            break;
        }
        if let Some(field_value) = map.get(field) {
            validate_value_against_type_at(
                field_value,
                expected_ty,
                format!(r#"{path}["{field}"]"#),
                issues,
            );
        } else {
            issues.push(ValidationIssue {
                path: path.clone(),
                kind: ValidationIssueKind::MissingField {
                    field: field.clone(),
                    expected: expected_ty.clone(),
                },
            });
        }
    }

    for (field, field_value) in map {
        if issues.len() >= MAX_ISSUES {
            break;
        }
        if expected_fields.contains_key(field) {
            continue;
        }
        match tail {
            RowTail::Closed => issues.push(ValidationIssue {
                path: path.clone(),
                kind: ValidationIssueKind::UnexpectedField {
                    field: field.clone(),
                },
            }),
            RowTail::Open(rest) => validate_value_against_type_at(
                field_value,
                rest,
                format!(r#"{path}["{field}"]"#),
                issues,
            ),
            RowTail::Var(_) => {}
        }
    }
}

fn push_mismatch(path: String, expected: Type, value: &Value, issues: &mut Vec<ValidationIssue>) {
    if issues.len() >= MAX_ISSUES {
        return;
    }

    issues.push(ValidationIssue {
        path,
        kind: ValidationIssueKind::TypeMismatch {
            expected,
            actual: preview_type(value).normalize(),
        },
    });
}

fn preview_type(value: &Value) -> Type {
    match value {
        Value::Null => Type::Null,
        Value::Bool(b) => Type::BoolLiteral(*b),
        Value::Number(n) => Type::NumberLiteral(n.to_string()),
        Value::String(s) => {
            if s.len() <= MAX_PREVIEW_STRING_LITERAL_LEN {
                Type::StringLiteral(s.clone())
            } else {
                Type::String
            }
        }
        Value::Array(items) => preview_array_type(items),
        Value::Object(map) => preview_object_type(map),
    }
}

fn preview_array_type(items: &[Value]) -> Type {
    if items.is_empty() {
        return Type::Tuple(Vec::new());
    }

    if items.len() <= MAX_PREVIEW_ARRAY_ITEMS {
        return Type::Tuple(items.iter().map(preview_type).collect()).normalize();
    }

    let element =
        Type::union(items.iter().take(MAX_PREVIEW_ARRAY_ITEMS).map(preview_type)).normalize();
    Type::NonEmptyArray(Box::new(element)).normalize()
}

fn preview_object_type(map: &serde_json::Map<String, Value>) -> Type {
    if map.len() <= MAX_PREVIEW_OBJECT_FIELDS {
        let fields = map
            .iter()
            .map(|(k, v)| (k.clone(), preview_type(v)))
            .collect();
        return Type::Object(ObjectShape::closed(fields)).normalize();
    }

    let mut fields = BTreeMap::new();
    for (k, v) in map.iter().take(MAX_PREVIEW_OBJECT_FIELDS) {
        fields.insert(k.clone(), preview_type(v));
    }

    Type::Object(ObjectShape {
        fields,
        tail: RowTail::Open(Box::new(Type::Any)),
    })
    .normalize()
}

#[cfg(test)]
mod tests {
    use super::{ValidationIssueKind, validate_value_against_type};
    use crate::minijq::types::{ObjectShape, Type};
    use serde_json::json;

    #[test]
    fn reports_nested_field_mismatch() {
        let expected = Type::Object(ObjectShape::with_required_field(
            "metrics",
            Type::Object(ObjectShape::with_required_field("a", Type::Number)),
        ));
        let input = json!({"metrics": {"a": "oops"}});
        let issues = validate_value_against_type(&input, &expected);
        assert!(!issues.is_empty());
        let has_nested = issues.iter().any(|issue| {
            issue.path == "$[\"metrics\"][\"a\"]"
                && matches!(issue.kind, ValidationIssueKind::TypeMismatch { .. })
        });
        assert!(has_nested, "expected nested mismatch, got: {:?}", issues);
    }

    #[test]
    fn reports_non_empty_array_requirement() {
        let expected = Type::NonEmptyArray(Box::new(Type::Number));
        let input = json!([]);
        let issues = validate_value_against_type(&input, &expected);
        assert!(
            issues
                .iter()
                .any(|issue| { matches!(issue.kind, ValidationIssueKind::NonEmptyRequired) })
        );
    }
}
