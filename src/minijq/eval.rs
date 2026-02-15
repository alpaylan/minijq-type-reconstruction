use crate::minijq::ast::{BinaryOp, Expr, UnaryOp};
use crate::minijq::types::{ObjectShape, Type};
use crate::minijq::typing::Builtin;
use serde_json::{Map, Number, Value};
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeTypeError {
    pub op: String,
    pub expected: Type,
    pub actual: Type,
    pub input_type: Type,
    pub message: String,
}

impl fmt::Display for RuntimeTypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.op == "error" {
            return write!(f, "{}", self.message);
        }
        write!(
            f,
            "{}: expected {}, got {} (input {})",
            self.op, self.expected, self.actual, self.input_type
        )
    }
}

impl std::error::Error for RuntimeTypeError {}

pub fn eval(expr: &Expr, input: &Value) -> Result<Value, RuntimeTypeError> {
    eval_inner(expr, input, input)
}

fn eval_inner(expr: &Expr, current: &Value, root: &Value) -> Result<Value, RuntimeTypeError> {
    match expr {
        Expr::Identity => Ok(current.clone()),
        Expr::Literal(value) => Ok(value.clone()),
        Expr::Pipe(lhs, rhs) => {
            let mid = eval_inner(lhs, current, root)?;
            eval_inner(rhs, &mid, root)
        }
        Expr::Binary(op, lhs, rhs) => eval_binary(*op, lhs, rhs, current, root),
        Expr::Unary(op, inner) => {
            let value = eval_inner(inner, current, root)?;
            eval_unary(*op, &value, root)
        }
        Expr::Builtin(Builtin::Map, inner) => eval_map_builtin(inner, current, root),
        Expr::Builtin(Builtin::Select, inner) => eval_select_builtin(inner, current, root),
        Expr::Builtin(op, inner) => {
            let argument = eval_inner(inner, current, root)?;
            eval_builtin(*op, &argument, current, root)
        }
        Expr::Field(inner, field) => {
            let value = eval_inner(inner, current, root)?;
            match value {
                Value::Object(map) => Ok(map.get(field).cloned().unwrap_or(Value::Null)),
                other => Err(type_error(
                    "field",
                    Type::Object(ObjectShape::open_any()),
                    &other,
                    root,
                )),
            }
        }
        Expr::Index(inner, index) => {
            let value = eval_inner(inner, current, root)?;
            match value {
                Value::Array(items) => {
                    if *index == 0 && items.is_empty() {
                        return Err(type_error(
                            "index",
                            Type::NonEmptyArray(Box::new(Type::Any)),
                            &Value::Array(items),
                            root,
                        ));
                    }
                    Ok(items.get(*index).cloned().unwrap_or(Value::Null))
                }
                other => Err(type_error(
                    "index",
                    Type::Array(Box::new(Type::Any)),
                    &other,
                    root,
                )),
            }
        }
        Expr::Lookup(inner, key) => {
            let container = eval_inner(inner, current, root)?;
            let key_value = eval_inner(key, current, root)?;
            eval_lookup(&container, &key_value, root)
        }
        Expr::Optional(inner) => match eval_inner(inner, current, root) {
            Ok(value) => Ok(value),
            Err(_) => Ok(Value::Null),
        },
        Expr::TryCatch {
            try_expr,
            catch_expr,
        } => match eval_inner(try_expr, current, root) {
            Ok(value) => Ok(value),
            Err(_) => eval_inner(catch_expr, current, root),
        },
        Expr::IfElse {
            cond,
            then_branch,
            else_branch,
        } => {
            let condition = eval_inner(cond, current, root)?;
            if is_truthy(&condition) {
                eval_inner(then_branch, current, root)
            } else {
                eval_inner(else_branch, current, root)
            }
        }
        Expr::ArrayLiteral(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(eval_inner(item, current, root)?);
            }
            Ok(Value::Array(out))
        }
        Expr::ObjectLiteral(fields) => {
            let mut out = Map::new();
            for (k, v) in fields {
                out.insert(k.clone(), eval_inner(v, current, root)?);
            }
            Ok(Value::Object(out))
        }
    }
}

fn eval_binary(
    op: BinaryOp,
    lhs: &Expr,
    rhs: &Expr,
    current: &Value,
    root: &Value,
) -> Result<Value, RuntimeTypeError> {
    match op {
        BinaryOp::And => {
            let left = eval_inner(lhs, current, root)?;
            if !is_truthy(&left) {
                return Ok(Value::Bool(false));
            }
            let right = eval_inner(rhs, current, root)?;
            Ok(Value::Bool(is_truthy(&right)))
        }
        BinaryOp::Or => {
            let left = eval_inner(lhs, current, root)?;
            if is_truthy(&left) {
                return Ok(Value::Bool(true));
            }
            let right = eval_inner(rhs, current, root)?;
            Ok(Value::Bool(is_truthy(&right)))
        }
        BinaryOp::Alt => {
            let left = eval_inner(lhs, current, root)?;
            if !is_null_or_false(&left) {
                return Ok(left);
            }
            eval_inner(rhs, current, root)
        }
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
            let left = eval_inner(lhs, current, root)?;
            let right = eval_inner(rhs, current, root)?;
            let left_num = left
                .as_f64()
                .ok_or_else(|| type_error(op.symbol(), Type::Number, &left, root))?;
            let right_num = right
                .as_f64()
                .ok_or_else(|| type_error(op.symbol(), Type::Number, &right, root))?;

            let out = match op {
                BinaryOp::Add => left_num + right_num,
                BinaryOp::Sub => left_num - right_num,
                BinaryOp::Mul => left_num * right_num,
                BinaryOp::Div => left_num / right_num,
                _ => unreachable!(),
            };

            let number = Number::from_f64(out)
                .unwrap_or_else(|| panic!("non-finite arithmetic result: {out}"));
            Ok(Value::Number(number))
        }
        BinaryOp::Eq | BinaryOp::Ne => {
            let left = eval_inner(lhs, current, root)?;
            let right = eval_inner(rhs, current, root)?;
            let eq = left == right;
            Ok(Value::Bool(if matches!(op, BinaryOp::Eq) {
                eq
            } else {
                !eq
            }))
        }
        BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => {
            let left = eval_inner(lhs, current, root)?;
            let right = eval_inner(rhs, current, root)?;
            let result = compare_ordering(op, Some(json_order(&left, &right)));
            Ok(Value::Bool(result))
        }
    }
}

fn compare_ordering(op: BinaryOp, ordering: Option<std::cmp::Ordering>) -> bool {
    let Some(ord) = ordering else {
        return false;
    };

    match op {
        BinaryOp::Lt => ord.is_lt(),
        BinaryOp::Lte => ord.is_le(),
        BinaryOp::Gt => ord.is_gt(),
        BinaryOp::Gte => ord.is_ge(),
        _ => false,
    }
}

fn json_type_rank(value: &Value) -> u8 {
    match value {
        Value::Null => 0,
        Value::Bool(_) => 1,
        Value::Number(_) => 2,
        Value::String(_) => 3,
        Value::Array(_) => 4,
        Value::Object(_) => 5,
    }
}

fn json_order(a: &Value, b: &Value) -> std::cmp::Ordering {
    let ra = json_type_rank(a);
    let rb = json_type_rank(b);
    if ra != rb {
        return ra.cmp(&rb);
    }

    match (a, b) {
        (Value::Null, Value::Null) => std::cmp::Ordering::Equal,
        (Value::Bool(x), Value::Bool(y)) => x.cmp(y),
        (Value::Number(x), Value::Number(y)) => {
            let xf = x.as_f64().unwrap_or(0.0);
            let yf = y.as_f64().unwrap_or(0.0);
            xf.partial_cmp(&yf).unwrap_or(std::cmp::Ordering::Equal)
        }
        (Value::String(x), Value::String(y)) => x.cmp(y),
        (Value::Array(xs), Value::Array(ys)) => {
            for (x, y) in xs.iter().zip(ys.iter()) {
                let ord = json_order(x, y);
                if ord != std::cmp::Ordering::Equal {
                    return ord;
                }
            }
            xs.len().cmp(&ys.len())
        }
        (Value::Object(xm), Value::Object(ym)) => {
            let mut x_entries: Vec<(&String, &Value)> = xm.iter().collect();
            let mut y_entries: Vec<(&String, &Value)> = ym.iter().collect();
            x_entries.sort_by(|(ka, _), (kb, _)| ka.cmp(kb));
            y_entries.sort_by(|(ka, _), (kb, _)| ka.cmp(kb));

            for ((kx, vx), (ky, vy)) in x_entries.iter().zip(y_entries.iter()) {
                let key_ord = kx.cmp(ky);
                if key_ord != std::cmp::Ordering::Equal {
                    return key_ord;
                }
                let val_ord = json_order(vx, vy);
                if val_ord != std::cmp::Ordering::Equal {
                    return val_ord;
                }
            }
            x_entries.len().cmp(&y_entries.len())
        }
        _ => std::cmp::Ordering::Equal,
    }
}

fn eval_unary(op: UnaryOp, value: &Value, root: &Value) -> Result<Value, RuntimeTypeError> {
    match op {
        UnaryOp::Not => Ok(Value::Bool(!is_truthy(value))),
        UnaryOp::Neg => {
            let n = value
                .as_f64()
                .ok_or_else(|| type_error("neg", Type::Number, value, root))?;
            let out = Number::from_f64(-n).unwrap_or_else(|| panic!("non-finite negate result"));
            Ok(Value::Number(out))
        }
    }
}

fn eval_builtin(
    op: Builtin,
    argument: &Value,
    current: &Value,
    root: &Value,
) -> Result<Value, RuntimeTypeError> {
    match op {
        Builtin::Error => Err(explicit_error(argument, root)),
        Builtin::Length => match argument {
            Value::String(s) => Ok(Value::Number(Number::from(s.chars().count() as u64))),
            Value::Array(items) => Ok(Value::Number(Number::from(items.len() as u64))),
            Value::Object(map) => Ok(Value::Number(Number::from(map.len() as u64))),
            other => Err(type_error("length", op.expected_input_type(), other, root)),
        },
        Builtin::First => match argument {
            Value::Array(items) if !items.is_empty() => Ok(items[0].clone()),
            Value::Array(_) => Err(type_error(
                "first",
                op.expected_input_type(),
                argument,
                root,
            )),
            other => Err(type_error("first", op.expected_input_type(), other, root)),
        },
        Builtin::Last => match argument {
            Value::Array(items) if !items.is_empty() => {
                Ok(items.last().cloned().unwrap_or(Value::Null))
            }
            Value::Array(_) => Err(type_error("last", op.expected_input_type(), argument, root)),
            other => Err(type_error("last", op.expected_input_type(), other, root)),
        },
        Builtin::Keys => match argument {
            Value::Object(map) => {
                let mut keys = map.keys().cloned().collect::<Vec<_>>();
                keys.sort();
                Ok(Value::Array(keys.into_iter().map(Value::String).collect()))
            }
            other => Err(type_error("keys", op.expected_input_type(), other, root)),
        },
        Builtin::TypeName => Ok(Value::String(type_name(argument).to_string())),
        Builtin::Has => eval_has_builtin(argument, current, root),
        Builtin::Contains => Ok(Value::Bool(json_contains(current, argument))),
        Builtin::StartsWith => match current {
            Value::String(s) => match argument {
                Value::String(prefix) => Ok(Value::Bool(s.starts_with(prefix))),
                other => Err(type_error("startswith", Type::String, other, root)),
            },
            other => Err(type_error("startswith", Type::String, other, root)),
        },
        Builtin::EndsWith => match current {
            Value::String(s) => match argument {
                Value::String(suffix) => Ok(Value::Bool(s.ends_with(suffix))),
                other => Err(type_error("endswith", Type::String, other, root)),
            },
            other => Err(type_error("endswith", Type::String, other, root)),
        },
        Builtin::Reverse => match argument {
            Value::Array(items) => {
                let mut out = items.clone();
                out.reverse();
                Ok(Value::Array(out))
            }
            other => Err(type_error("reverse", op.expected_input_type(), other, root)),
        },
        Builtin::Sort => match argument {
            Value::Array(items) => {
                let mut out = items.clone();
                out.sort_by(json_order);
                Ok(Value::Array(out))
            }
            other => Err(type_error("sort", op.expected_input_type(), other, root)),
        },
        Builtin::ToString => match argument {
            Value::String(s) => Ok(Value::String(s.clone())),
            other => Ok(Value::String(other.to_string())),
        },
        Builtin::ToNumber => match argument {
            Value::Number(n) => Ok(Value::Number(n.clone())),
            Value::String(s) => {
                let parsed = s.parse::<f64>().map_err(|_| RuntimeTypeError {
                    op: "tonumber".to_string(),
                    expected: Type::Number,
                    actual: Type::String,
                    input_type: Type::from_json_value(root),
                    message: format!("tonumber: invalid numeric string `{s}`"),
                })?;
                let number = Number::from_f64(parsed).ok_or(RuntimeTypeError {
                    op: "tonumber".to_string(),
                    expected: Type::Number,
                    actual: Type::String,
                    input_type: Type::from_json_value(root),
                    message: "tonumber: parsed non-finite number".to_string(),
                })?;
                Ok(Value::Number(number))
            }
            other => Err(type_error(
                "tonumber",
                op.expected_input_type(),
                other,
                root,
            )),
        },
        Builtin::Abs => {
            let n = argument
                .as_f64()
                .ok_or_else(|| type_error("abs", Type::Number, argument, root))?;
            let out = Number::from_f64(n.abs()).ok_or(RuntimeTypeError {
                op: "abs".to_string(),
                expected: Type::Number,
                actual: Type::Number,
                input_type: Type::from_json_value(root),
                message: "abs: produced non-finite number".to_string(),
            })?;
            Ok(Value::Number(out))
        }
        Builtin::Floor => {
            let n = argument
                .as_f64()
                .ok_or_else(|| type_error("floor", Type::Number, argument, root))?;
            let out = Number::from_f64(n.floor()).ok_or(RuntimeTypeError {
                op: "floor".to_string(),
                expected: Type::Number,
                actual: Type::Number,
                input_type: Type::from_json_value(root),
                message: "floor: produced non-finite number".to_string(),
            })?;
            Ok(Value::Number(out))
        }
        Builtin::Ceil => {
            let n = argument
                .as_f64()
                .ok_or_else(|| type_error("ceil", Type::Number, argument, root))?;
            let out = Number::from_f64(n.ceil()).ok_or(RuntimeTypeError {
                op: "ceil".to_string(),
                expected: Type::Number,
                actual: Type::Number,
                input_type: Type::from_json_value(root),
                message: "ceil: produced non-finite number".to_string(),
            })?;
            Ok(Value::Number(out))
        }
        Builtin::Add => eval_add_builtin(argument, root),
        Builtin::Min => eval_min_max_builtin("min", argument, root, true),
        Builtin::Max => eval_min_max_builtin("max", argument, root, false),
        Builtin::Split => match current {
            Value::String(s) => match argument {
                Value::String(sep) => {
                    let parts = if sep.is_empty() {
                        s.chars().map(|ch| Value::String(ch.to_string())).collect()
                    } else {
                        s.split(sep)
                            .map(|part| Value::String(part.to_string()))
                            .collect()
                    };
                    Ok(Value::Array(parts))
                }
                other => Err(type_error("split", Type::String, other, root)),
            },
            other => Err(type_error("split", Type::String, other, root)),
        },
        Builtin::Join => match current {
            Value::Array(items) => match argument {
                Value::String(sep) => {
                    let mut parts = Vec::with_capacity(items.len());
                    for item in items {
                        let text = match item {
                            Value::String(s) => s.clone(),
                            Value::Number(n) => n.to_string(),
                            Value::Bool(b) => b.to_string(),
                            Value::Null => String::new(),
                            other => {
                                return Err(type_error(
                                    "join",
                                    Type::union(vec![
                                        Type::String,
                                        Type::Number,
                                        Type::Bool,
                                        Type::Null,
                                    ]),
                                    other,
                                    root,
                                ));
                            }
                        };
                        parts.push(text);
                    }
                    Ok(Value::String(parts.join(sep)))
                }
                other => Err(type_error("join", Type::String, other, root)),
            },
            other => Err(type_error(
                "join",
                Type::Array(Box::new(Type::String)),
                other,
                root,
            )),
        },
        Builtin::Map | Builtin::Select => unreachable!("handled before eval_builtin"),
    }
}

fn eval_add_builtin(argument: &Value, root: &Value) -> Result<Value, RuntimeTypeError> {
    let Value::Array(items) = argument else {
        return Err(type_error(
            "add",
            Type::Array(Box::new(Type::Any)),
            argument,
            root,
        ));
    };

    if items.is_empty() {
        return Ok(Value::Null);
    }

    match &items[0] {
        Value::Number(_) => {
            let mut sum = 0.0;
            for item in items {
                let n = item
                    .as_f64()
                    .ok_or_else(|| type_error("add", Type::Number, item, root))?;
                sum += n;
            }
            let out = Number::from_f64(sum).ok_or(RuntimeTypeError {
                op: "add".to_string(),
                expected: Type::Number,
                actual: Type::Number,
                input_type: Type::from_json_value(root),
                message: "add: produced non-finite number".to_string(),
            })?;
            Ok(Value::Number(out))
        }
        Value::String(_) => {
            let mut out = String::new();
            for item in items {
                let Some(text) = item.as_str() else {
                    return Err(type_error("add", Type::String, item, root));
                };
                out.push_str(text);
            }
            Ok(Value::String(out))
        }
        Value::Array(_) => {
            let mut out = Vec::new();
            for item in items {
                let Value::Array(arr) = item else {
                    return Err(type_error(
                        "add",
                        Type::Array(Box::new(Type::Any)),
                        item,
                        root,
                    ));
                };
                out.extend(arr.clone());
            }
            Ok(Value::Array(out))
        }
        Value::Object(_) => {
            let mut out = Map::new();
            for item in items {
                let Value::Object(map) = item else {
                    return Err(type_error(
                        "add",
                        Type::Object(ObjectShape::open_any()),
                        item,
                        root,
                    ));
                };
                for (k, v) in map {
                    out.insert(k.clone(), v.clone());
                }
            }
            Ok(Value::Object(out))
        }
        other => Err(type_error(
            "add",
            Type::union(vec![
                Type::Number,
                Type::String,
                Type::Array(Box::new(Type::Any)),
                Type::Object(ObjectShape::open_any()),
            ]),
            other,
            root,
        )),
    }
}

fn eval_min_max_builtin(
    op: &str,
    argument: &Value,
    root: &Value,
    find_min: bool,
) -> Result<Value, RuntimeTypeError> {
    let Value::Array(items) = argument else {
        return Err(type_error(
            op,
            Type::Array(Box::new(Type::Any)),
            argument,
            root,
        ));
    };
    if items.is_empty() {
        return Err(type_error(
            op,
            Type::NonEmptyArray(Box::new(Type::Any)),
            argument,
            root,
        ));
    }

    let mut best = items[0].clone();
    for item in items.iter().skip(1) {
        let ord = json_order(item, &best);
        let replace = if find_min { ord.is_lt() } else { ord.is_gt() };
        if replace {
            best = item.clone();
        }
    }
    Ok(best)
}

fn eval_lookup(container: &Value, key: &Value, root: &Value) -> Result<Value, RuntimeTypeError> {
    match container {
        Value::Object(map) => match key {
            Value::String(field) => Ok(map.get(field).cloned().unwrap_or(Value::Null)),
            other => Err(type_error("lookup", Type::String, other, root)),
        },
        Value::Array(items) => {
            let Some(index) = key_to_index(key) else {
                return Err(type_error("lookup", Type::Number, key, root));
            };
            if index == 0 && items.is_empty() {
                return Err(type_error(
                    "lookup",
                    Type::NonEmptyArray(Box::new(Type::Any)),
                    container,
                    root,
                ));
            }
            Ok(items.get(index).cloned().unwrap_or(Value::Null))
        }
        other => Err(type_error(
            "lookup",
            Type::union(vec![
                Type::Object(ObjectShape::open_any()),
                Type::Array(Box::new(Type::Any)),
            ]),
            other,
            root,
        )),
    }
}

fn eval_has_builtin(
    key_value: &Value,
    current: &Value,
    root: &Value,
) -> Result<Value, RuntimeTypeError> {
    match current {
        Value::Object(map) => match key_value {
            Value::String(key) => Ok(Value::Bool(map.contains_key(key))),
            _ => Ok(Value::Bool(false)),
        },
        Value::Array(items) => {
            let maybe_index = key_to_index(key_value);
            Ok(Value::Bool(
                maybe_index.is_some_and(|index| index < items.len()),
            ))
        }
        other => Err(type_error(
            "has",
            Type::union(vec![
                Type::Object(ObjectShape::open_any()),
                Type::Array(Box::new(Type::Any)),
            ]),
            other,
            root,
        )),
    }
}

fn key_to_index(value: &Value) -> Option<usize> {
    let n = value.as_f64()?;
    if !n.is_finite() || n < 0.0 || n.fract() != 0.0 {
        return None;
    }
    let idx = n as u64;
    usize::try_from(idx).ok()
}

fn json_contains(container: &Value, needle: &Value) -> bool {
    match (container, needle) {
        (Value::String(haystack), Value::String(sub)) => haystack.contains(sub),
        (Value::Array(haystack), Value::Array(items)) => items.iter().all(|needle_item| {
            haystack
                .iter()
                .any(|hay_item| json_contains(hay_item, needle_item))
        }),
        (Value::Object(haystack), Value::Object(items)) => items.iter().all(|(k, needle_value)| {
            haystack
                .get(k)
                .is_some_and(|hay_value| json_contains(hay_value, needle_value))
        }),
        _ => container == needle,
    }
}

fn eval_map_builtin(
    mapper: &Expr,
    current: &Value,
    root: &Value,
) -> Result<Value, RuntimeTypeError> {
    let Value::Array(items) = current else {
        return Err(type_error(
            "map",
            Type::Array(Box::new(Type::Any)),
            current,
            root,
        ));
    };

    let mut out = Vec::with_capacity(items.len());
    for item in items {
        out.push(eval_inner(mapper, item, root)?);
    }
    Ok(Value::Array(out))
}

fn eval_select_builtin(
    predicate: &Expr,
    current: &Value,
    root: &Value,
) -> Result<Value, RuntimeTypeError> {
    let pred = eval_inner(predicate, current, root)?;
    if is_truthy(&pred) {
        Ok(current.clone())
    } else {
        Ok(Value::Null)
    }
}

fn type_name(value: &Value) -> &'static str {
    match value {
        Value::Null => "null",
        Value::Bool(_) => "boolean",
        Value::Number(_) => "number",
        Value::String(_) => "string",
        Value::Array(_) => "array",
        Value::Object(_) => "object",
    }
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Null | Value::Bool(false))
}

fn is_null_or_false(value: &Value) -> bool {
    matches!(value, Value::Null | Value::Bool(false))
}

fn type_error(op: &str, expected: Type, actual_value: &Value, root: &Value) -> RuntimeTypeError {
    let actual = Type::from_json_value(actual_value);
    let input_type = Type::from_json_value(root);
    let message = format!("{op}: expected {expected}, got {actual}");
    RuntimeTypeError {
        op: op.to_string(),
        expected,
        actual: actual.clone(),
        input_type,
        message,
    }
}

fn explicit_error(argument: &Value, root: &Value) -> RuntimeTypeError {
    let actual = Type::from_json_value(argument);
    let rendered = match argument {
        Value::String(s) => s.clone(),
        other => format!("(not a string): {}", other),
    };
    RuntimeTypeError {
        op: "error".to_string(),
        expected: Type::Any,
        actual,
        input_type: Type::from_json_value(root),
        message: format!("error: {rendered}"),
    }
}

#[cfg(test)]
mod tests {
    use super::eval;
    use crate::minijq::ast::Expr;
    use crate::minijq::types::Type;
    use crate::minijq::typing::Builtin;
    use serde_json::json;

    #[test]
    fn add_self_on_number() {
        let expr = Expr::add(Expr::identity(), Expr::identity());
        let result = eval(&expr, &json!(3.0));
        let value = result.expect("must succeed");
        assert_eq!(value.as_f64(), Some(6.0));
    }

    #[test]
    fn arithmetic_errors_on_string() {
        let expr = Expr::mul(Expr::identity(), Expr::literal(json!(2)));
        let err = eval(&expr, &json!("abc")).expect_err("must fail");
        assert_eq!(err.expected, Type::Number);
        assert_eq!(err.actual, Type::String);
    }

    #[test]
    fn comparison_on_numbers() {
        let expr = Expr::gte(Expr::identity(), Expr::literal(json!(3)));
        assert_eq!(eval(&expr, &json!(4)).expect("pass"), json!(true));
    }

    #[test]
    fn comparison_on_mixed_types_is_defined() {
        let expr = Expr::gt(Expr::identity(), Expr::literal(json!(3)));
        let out = eval(&expr, &json!("x")).expect("comparison should be defined");
        assert_eq!(out, json!(true));
    }

    #[test]
    fn if_else_uses_truthiness() {
        let expr = Expr::if_else(
            Expr::identity(),
            Expr::literal(json!(1)),
            Expr::literal(json!(0)),
        );
        assert_eq!(eval(&expr, &json!(false)).expect("pass"), json!(0));
        assert_eq!(eval(&expr, &json!("x")).expect("pass"), json!(1));
    }

    #[test]
    fn alt_operator_uses_rhs_for_null() {
        let expr = Expr::alt(Expr::identity(), Expr::literal(json!("fallback")));
        assert_eq!(eval(&expr, &json!(null)).expect("pass"), json!("fallback"));
        assert_eq!(eval(&expr, &json!(5)).expect("pass"), json!(5));
    }

    #[test]
    fn keys_returns_sorted_keys() {
        let expr = Expr::builtin(Builtin::Keys, Expr::identity());
        assert_eq!(
            eval(&expr, &json!({"b": 1, "a": 2})).expect("pass"),
            json!(["a", "b"])
        );
    }

    #[test]
    fn type_builtin_returns_name() {
        let expr = Expr::builtin(Builtin::TypeName, Expr::identity());
        assert_eq!(eval(&expr, &json!([1, 2])).expect("pass"), json!("array"));
    }

    #[test]
    fn map_applies_expression_per_element() {
        let expr = Expr::builtin(
            Builtin::Map,
            Expr::add(Expr::identity(), Expr::literal(json!(1))),
        );
        let out = eval(&expr, &json!([1, 2, 3])).expect("pass");
        let items = out.as_array().expect("map output must be array");
        assert_eq!(items.len(), 3);
        assert_eq!(items[0].as_f64(), Some(2.0));
        assert_eq!(items[1].as_f64(), Some(3.0));
        assert_eq!(items[2].as_f64(), Some(4.0));
    }

    #[test]
    fn map_errors_on_non_array() {
        let expr = Expr::builtin(Builtin::Map, Expr::identity());
        let err = eval(&expr, &json!(5)).expect_err("must fail");
        assert_eq!(err.expected, Type::Array(Box::new(Type::Any)));
    }

    #[test]
    fn has_checks_object_key() {
        let expr = Expr::builtin(Builtin::Has, Expr::literal(json!("a")));
        assert_eq!(eval(&expr, &json!({"a": 1})).expect("pass"), json!(true));
        assert_eq!(eval(&expr, &json!({"b": 1})).expect("pass"), json!(false));
    }

    #[test]
    fn has_checks_array_index() {
        let expr = Expr::builtin(Builtin::Has, Expr::literal(json!(1)));
        assert_eq!(eval(&expr, &json!(["x", "y"])).expect("pass"), json!(true));
        assert_eq!(eval(&expr, &json!(["x"])).expect("pass"), json!(false));
    }

    #[test]
    fn has_errors_on_invalid_input_kind() {
        let expr = Expr::builtin(Builtin::Has, Expr::literal(json!("x")));
        let err = eval(&expr, &json!(10)).expect_err("must fail");
        assert_eq!(err.actual, Type::Number);
    }

    #[test]
    fn contains_supports_recursive_object_and_array_checks() {
        let object_expr = Expr::builtin(Builtin::Contains, Expr::literal(json!({"a": 1})));
        assert_eq!(
            eval(&object_expr, &json!({"a": 1, "b": 2})).expect("pass"),
            json!(true)
        );

        let array_expr = Expr::builtin(Builtin::Contains, Expr::literal(json!([2, 3])));
        assert_eq!(
            eval(&array_expr, &json!([1, 2, 3, 4])).expect("pass"),
            json!(true)
        );
    }

    #[test]
    fn startswith_and_endswith_on_strings() {
        let starts = Expr::builtin(Builtin::StartsWith, Expr::literal(json!("pre")));
        let ends = Expr::builtin(Builtin::EndsWith, Expr::literal(json!("fix")));
        assert_eq!(eval(&starts, &json!("prefix")).expect("pass"), json!(true));
        assert_eq!(eval(&ends, &json!("prefix")).expect("pass"), json!(true));
    }

    #[test]
    fn startswith_errors_on_non_string_input() {
        let expr = Expr::builtin(Builtin::StartsWith, Expr::literal(json!("a")));
        let err = eval(&expr, &json!(5)).expect_err("must fail");
        assert_eq!(err.expected, Type::String);
    }

    #[test]
    fn reverse_and_sort_arrays() {
        let reverse = Expr::builtin(Builtin::Reverse, Expr::identity());
        let sort = Expr::builtin(Builtin::Sort, Expr::identity());

        assert_eq!(
            eval(&reverse, &json!([1, 2, 3])).expect("reverse pass"),
            json!([3, 2, 1])
        );
        assert_eq!(
            eval(&sort, &json!([3, 1, 2])).expect("sort pass"),
            json!([1, 2, 3])
        );
    }

    #[test]
    fn lookup_reads_dynamic_object_key() {
        let expr = Expr::lookup(
            Expr::field(Expr::identity(), "data"),
            Expr::field(Expr::identity(), "key"),
        );
        let out = eval(&expr, &json!({"data": {"x": 7}, "key": "x"})).expect("pass");
        assert_eq!(out, json!(7));
    }

    #[test]
    fn lookup_reads_dynamic_array_index() {
        let expr = Expr::lookup(Expr::identity(), Expr::literal(json!(2)));
        let out = eval(&expr, &json!([10, 11, 12])).expect("pass");
        assert_eq!(out, json!(12));
    }

    #[test]
    fn optional_suppresses_runtime_type_errors() {
        let expr = Expr::optional(Expr::field(Expr::identity(), "name"));
        let out = eval(&expr, &json!(10)).expect("optional should suppress type errors");
        assert_eq!(out, json!(null));
    }

    #[test]
    fn tonumber_parses_numeric_strings() {
        let expr = Expr::builtin(Builtin::ToNumber, Expr::identity());
        let out = eval(&expr, &json!("12.5")).expect("pass");
        assert_eq!(out.as_f64(), Some(12.5));
    }

    #[test]
    fn tonumber_errors_for_non_numeric_string() {
        let expr = Expr::builtin(Builtin::ToNumber, Expr::identity());
        let err = eval(&expr, &json!("abc")).expect_err("must fail");
        assert_eq!(err.op, "tonumber");
    }

    #[test]
    fn explicit_error_builtin_produces_runtime_error() {
        let expr = Expr::builtin(Builtin::Error, Expr::identity());
        let err = eval(&expr, &json!(null)).expect_err("must fail");
        assert_eq!(err.op, "error");
        assert!(err.message.contains("(not a string): null"));
    }

    #[test]
    fn explicit_error_can_be_caught() {
        let expr = Expr::try_catch(
            Expr::builtin(Builtin::Error, Expr::identity()),
            Expr::literal(json!(0)),
        );
        assert_eq!(eval(&expr, &json!(true)).expect("pass"), json!(0));
    }

    #[test]
    fn boolean_guard_else_error() {
        let expr = Expr::if_else(
            Expr::or(
                Expr::eq(Expr::identity(), Expr::literal(json!(true))),
                Expr::eq(Expr::identity(), Expr::literal(json!(false))),
            ),
            Expr::literal(json!(1)),
            Expr::builtin(Builtin::Error, Expr::identity()),
        );
        assert_eq!(eval(&expr, &json!(true)).expect("pass"), json!(1));
        assert_eq!(eval(&expr, &json!(false)).expect("pass"), json!(1));
        let err = eval(&expr, &json!(null)).expect_err("must fail");
        assert_eq!(err.op, "error");
    }

    #[test]
    fn tostring_abs_floor_and_ceil_work() {
        let tostring = Expr::builtin(Builtin::ToString, Expr::identity());
        let abs = Expr::builtin(Builtin::Abs, Expr::identity());
        let floor = Expr::builtin(Builtin::Floor, Expr::identity());
        let ceil = Expr::builtin(Builtin::Ceil, Expr::identity());

        assert_eq!(
            eval(&tostring, &json!({"a": 1})).expect("pass"),
            json!("{\"a\":1}")
        );
        assert_eq!(eval(&abs, &json!(-3.7)).expect("pass"), json!(3.7));
        assert_eq!(eval(&floor, &json!(3.7)).expect("pass"), json!(3.0));
        assert_eq!(eval(&ceil, &json!(3.1)).expect("pass"), json!(4.0));
    }

    #[test]
    fn add_builtin_handles_numbers_strings_arrays_and_objects() {
        let add = Expr::builtin(Builtin::Add, Expr::identity());
        assert_eq!(eval(&add, &json!([1, 2, 3])).expect("pass"), json!(6.0));
        assert_eq!(eval(&add, &json!(["a", "b"])).expect("pass"), json!("ab"));
        assert_eq!(
            eval(&add, &json!([[1], [2, 3]])).expect("pass"),
            json!([1, 2, 3])
        );
        assert_eq!(
            eval(&add, &json!([{"a": 1}, {"b": 2, "a": 3}])).expect("pass"),
            json!({"a": 3, "b": 2})
        );
        assert_eq!(eval(&add, &json!([])).expect("pass"), json!(null));
    }

    #[test]
    fn min_and_max_require_nonempty_arrays() {
        let min = Expr::builtin(Builtin::Min, Expr::identity());
        let max = Expr::builtin(Builtin::Max, Expr::identity());
        assert_eq!(eval(&min, &json!([3, 1, 2])).expect("pass"), json!(1));
        assert_eq!(eval(&max, &json!([3, 1, 2])).expect("pass"), json!(3));
        let err = eval(&min, &json!([])).expect_err("must fail");
        assert_eq!(err.expected, Type::NonEmptyArray(Box::new(Type::Any)));
    }

    #[test]
    fn split_and_join_round_trip() {
        let split = Expr::builtin(Builtin::Split, Expr::literal(json!(",")));
        let join = Expr::builtin(Builtin::Join, Expr::literal(json!("-")));
        let items = eval(&split, &json!("a,b,c")).expect("split pass");
        assert_eq!(items, json!(["a", "b", "c"]));
        let joined = eval(&join, &items).expect("join pass");
        assert_eq!(joined, json!("a-b-c"));
    }

    #[test]
    fn join_accepts_non_string_scalars_like_jq() {
        let join = Expr::builtin(Builtin::Join, Expr::literal(json!(",")));
        let out = eval(&join, &json!([1, true, null, "x"])).expect("join should pass");
        assert_eq!(out, json!("1,true,,x"));
    }

    #[test]
    fn try_catch_recovers_from_errors() {
        let expr = Expr::try_catch(Expr::field(Expr::identity(), "x"), Expr::literal(json!(0)));
        assert_eq!(eval(&expr, &json!(10)).expect("pass"), json!(0));
        assert_eq!(eval(&expr, &json!({"x": 7})).expect("pass"), json!(7));
    }

    #[test]
    fn select_returns_value_or_null() {
        let expr = Expr::builtin(
            Builtin::Select,
            Expr::gt(Expr::identity(), Expr::literal(json!(0))),
        );
        assert_eq!(eval(&expr, &json!(2)).expect("pass"), json!(2));
        assert_eq!(eval(&expr, &json!(-1)).expect("pass"), json!(null));
    }

    #[test]
    fn first_returns_first_value() {
        let expr = Expr::builtin(Builtin::First, Expr::identity());
        let out = eval(&expr, &json!([1, 2, 3])).expect("first should pass");
        assert_eq!(out, json!(1));
    }

    #[test]
    fn first_errors_on_empty_array() {
        let expr = Expr::builtin(Builtin::First, Expr::identity());
        let err = eval(&expr, &json!([])).expect_err("must fail");
        assert_eq!(err.expected, Type::NonEmptyArray(Box::new(Type::Any)));
    }

    #[test]
    fn index0_errors_on_empty_array() {
        let expr = Expr::index(Expr::identity(), 0);
        let err = eval(&expr, &json!([])).expect_err("must fail");
        assert_eq!(err.expected, Type::NonEmptyArray(Box::new(Type::Any)));
    }

    #[test]
    fn length_errors_on_bool() {
        let expr = Expr::builtin(Builtin::Length, Expr::identity());
        let err = eval(&expr, &json!(true)).expect_err("must fail");
        assert!(err.expected.is_subtype_of(&Type::Any));
        assert_eq!(err.actual, Type::Bool);
    }

    #[test]
    fn field_on_non_object_errors() {
        let expr = Expr::field(Expr::identity(), "x");
        let err = eval(&expr, &json!(10)).expect_err("must fail");
        assert_eq!(err.actual, Type::Number);
    }
}
