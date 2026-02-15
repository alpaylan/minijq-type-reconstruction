use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RowTail {
    Closed,
    Open(Box<Type>),
    Var(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ObjectShape {
    pub fields: BTreeMap<String, Type>,
    pub tail: RowTail,
}

impl ObjectShape {
    pub fn open_any() -> ObjectShape {
        ObjectShape {
            fields: BTreeMap::new(),
            tail: RowTail::Open(Box::new(Type::Any)),
        }
    }

    pub fn closed(fields: BTreeMap<String, Type>) -> ObjectShape {
        ObjectShape {
            fields,
            tail: RowTail::Closed,
        }
    }

    pub fn with_required_field(name: impl Into<String>, ty: Type) -> ObjectShape {
        let mut fields = BTreeMap::new();
        fields.insert(name.into(), ty);
        ObjectShape {
            fields,
            tail: RowTail::Open(Box::new(Type::Any)),
        }
    }

    pub fn with_required_field_row(
        name: impl Into<String>,
        ty: Type,
        row_var: impl Into<String>,
    ) -> ObjectShape {
        let mut fields = BTreeMap::new();
        fields.insert(name.into(), ty);
        ObjectShape {
            fields,
            tail: RowTail::Var(row_var.into()),
        }
    }

    pub fn is_open_any(&self) -> bool {
        self.fields.is_empty()
            && matches!(
                self.tail,
                RowTail::Open(ref t) if matches!(t.as_ref(), Type::Any)
            )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Type {
    Never,
    Any,
    Null,
    Bool,
    Number,
    String,
    Array(Box<Type>),
    NonEmptyArray(Box<Type>),
    Tuple(Vec<Type>),
    Object(ObjectShape),
    Union(Vec<Type>),
    Generic(String),
}

impl Type {
    pub fn any_object() -> Type {
        Type::Object(ObjectShape::open_any())
    }

    pub fn union(types: impl IntoIterator<Item = Type>) -> Type {
        Type::Union(types.into_iter().collect()).normalize()
    }

    pub fn is_never(&self) -> bool {
        matches!(self, Type::Never)
    }

    pub fn erase_generics(&self) -> Type {
        match self {
            Type::Generic(_) => Type::Any,
            Type::Array(inner) => Type::Array(Box::new(inner.erase_generics())),
            Type::NonEmptyArray(inner) => Type::NonEmptyArray(Box::new(inner.erase_generics())),
            Type::Tuple(items) => Type::Tuple(items.iter().map(Type::erase_generics).collect()),
            Type::Object(shape) => {
                let fields = shape
                    .fields
                    .iter()
                    .map(|(k, v)| (k.clone(), v.erase_generics()))
                    .collect();
                let tail = match &shape.tail {
                    RowTail::Closed => RowTail::Closed,
                    RowTail::Open(rest) => RowTail::Open(Box::new(rest.erase_generics())),
                    RowTail::Var(_) => RowTail::Open(Box::new(Type::Any)),
                };
                Type::Object(ObjectShape { fields, tail })
            }
            Type::Union(items) => Type::union(items.iter().map(Type::erase_generics)),
            other => other.clone(),
        }
    }

    pub fn normalize(self) -> Type {
        match self {
            Type::Array(inner) => Type::Array(Box::new(inner.normalize())),
            Type::NonEmptyArray(inner) => Type::NonEmptyArray(Box::new(inner.normalize())),
            Type::Tuple(items) => Type::Tuple(items.into_iter().map(Type::normalize).collect()),
            Type::Object(shape) => {
                let fields = shape
                    .fields
                    .into_iter()
                    .map(|(k, v)| (k, v.normalize()))
                    .collect();
                let tail = normalize_row_tail(shape.tail);
                Type::Object(ObjectShape { fields, tail })
            }
            Type::Union(items) => normalize_union(items),
            other => other,
        }
    }

    pub fn subtract(&self, rhs: &Type) -> Type {
        let lhs = self.clone().normalize();
        let rhs = rhs.clone().normalize();

        match (lhs, rhs) {
            (Type::Never, _) => Type::Never,
            (lhs, Type::Never) => lhs,
            (_, Type::Any) => Type::Never,
            (Type::Any, rhs) => {
                let universe = [
                    Type::Null,
                    Type::Bool,
                    Type::Number,
                    Type::String,
                    Type::Array(Box::new(Type::Any)),
                    Type::any_object(),
                ];
                Type::union(universe.into_iter().map(|t| t.subtract(&rhs)))
            }
            (Type::Union(items), rhs) => Type::union(items.into_iter().map(|t| t.subtract(&rhs))),
            (lhs, Type::Union(items)) => {
                items.into_iter().fold(lhs, |acc, item| acc.subtract(&item))
            }
            (Type::Generic(a), Type::Generic(b)) if a == b => Type::Never,
            (Type::Generic(name), _) => Type::Generic(name),
            (lhs, rhs) if lhs == rhs => Type::Never,
            (Type::Array(lhs), Type::Array(rhs)) => {
                let remaining = lhs.subtract(&rhs);
                if remaining.is_never() {
                    Type::Never
                } else {
                    Type::Array(Box::new(remaining)).normalize()
                }
            }
            (Type::Array(lhs), Type::NonEmptyArray(rhs)) => {
                Type::Array(Box::new(lhs.subtract(&rhs))).normalize()
            }
            (Type::NonEmptyArray(lhs), Type::Array(rhs)) => {
                if Type::NonEmptyArray(lhs.clone()).is_subtype_of(&Type::Array(rhs.clone())) {
                    Type::Never
                } else {
                    let remaining = lhs.subtract(&rhs);
                    if remaining.is_never() {
                        Type::Never
                    } else {
                        Type::NonEmptyArray(Box::new(remaining)).normalize()
                    }
                }
            }
            (Type::NonEmptyArray(lhs), Type::NonEmptyArray(rhs)) => {
                let remaining = lhs.subtract(&rhs);
                if remaining.is_never() {
                    Type::Never
                } else {
                    Type::NonEmptyArray(Box::new(remaining)).normalize()
                }
            }
            (Type::Array(lhs), Type::Tuple(rhs_items)) => {
                if rhs_items.is_empty() {
                    Type::NonEmptyArray(lhs).normalize()
                } else if rhs_items.iter().all(|item| item.is_subtype_of(&lhs)) {
                    Type::Array(lhs)
                } else {
                    Type::Array(lhs)
                }
            }
            (Type::NonEmptyArray(lhs), Type::Tuple(rhs_items)) => {
                if rhs_items.is_empty() {
                    Type::NonEmptyArray(lhs)
                } else if rhs_items.iter().all(|item| item.is_subtype_of(&lhs)) {
                    Type::NonEmptyArray(lhs)
                } else {
                    Type::NonEmptyArray(lhs)
                }
            }
            (Type::Tuple(lhs_items), Type::Array(rhs_elem)) => {
                if lhs_items.iter().all(|item| item.is_subtype_of(&rhs_elem)) {
                    Type::Never
                } else {
                    Type::Tuple(lhs_items)
                }
            }
            (Type::Tuple(lhs_items), Type::NonEmptyArray(rhs_elem)) => {
                if !lhs_items.is_empty()
                    && lhs_items.iter().all(|item| item.is_subtype_of(&rhs_elem))
                {
                    Type::Never
                } else {
                    Type::Tuple(lhs_items)
                }
            }
            (Type::Tuple(lhs_items), Type::Tuple(rhs_items)) => {
                if lhs_items == rhs_items {
                    Type::Never
                } else {
                    Type::Tuple(lhs_items)
                }
            }
            (Type::Object(lhs), Type::Object(rhs)) => subtract_object_shape(lhs, rhs),
            (lhs, _) => lhs,
        }
    }

    pub fn intersect(&self, rhs: &Type) -> Type {
        let lhs = self.clone().normalize();
        let rhs = rhs.clone().normalize();

        match (lhs, rhs) {
            (Type::Never, _) | (_, Type::Never) => Type::Never,
            (lhs, Type::Any) | (Type::Any, lhs) => lhs,
            (Type::Union(items), rhs) => Type::union(items.into_iter().map(|t| t.intersect(&rhs))),
            (lhs, Type::Union(items)) => Type::union(items.into_iter().map(|t| lhs.intersect(&t))),
            (Type::Array(lhs), Type::Array(rhs)) => {
                Type::Array(Box::new(lhs.intersect(&rhs))).normalize()
            }
            (Type::NonEmptyArray(lhs), Type::NonEmptyArray(rhs)) => {
                let item = lhs.intersect(&rhs).normalize();
                if item.is_never() {
                    Type::Never
                } else {
                    Type::NonEmptyArray(Box::new(item)).normalize()
                }
            }
            (Type::Array(lhs), Type::NonEmptyArray(rhs))
            | (Type::NonEmptyArray(lhs), Type::Array(rhs)) => {
                let item = lhs.intersect(&rhs).normalize();
                if item.is_never() {
                    Type::Never
                } else {
                    Type::NonEmptyArray(Box::new(item)).normalize()
                }
            }
            (Type::Array(elem), Type::Tuple(items)) => intersect_array_tuple(&elem, &items),
            (Type::Tuple(items), Type::Array(elem)) => intersect_array_tuple(&elem, &items),
            (Type::NonEmptyArray(elem), Type::Tuple(items)) => {
                if items.is_empty() {
                    Type::Never
                } else {
                    intersect_array_tuple(&elem, &items)
                }
            }
            (Type::Tuple(items), Type::NonEmptyArray(elem)) => {
                if items.is_empty() {
                    Type::Never
                } else {
                    intersect_array_tuple(&elem, &items)
                }
            }
            (Type::Tuple(lhs), Type::Tuple(rhs)) => {
                if lhs.len() != rhs.len() {
                    Type::Never
                } else {
                    let mut out = Vec::with_capacity(lhs.len());
                    for (l, r) in lhs.iter().zip(rhs.iter()) {
                        let t = l.intersect(r).normalize();
                        if t.is_never() {
                            return Type::Never;
                        }
                        out.push(t);
                    }
                    Type::Tuple(out)
                }
            }
            (Type::Object(lhs), Type::Object(rhs)) => intersect_object_shape(lhs, rhs),
            (Type::Generic(_), rhs) | (rhs, Type::Generic(_)) => rhs,
            (lhs, rhs) if lhs == rhs => lhs,
            _ => Type::Never,
        }
    }

    pub fn is_subtype_of(&self, super_type: &Type) -> bool {
        let sub = self.clone().normalize();
        let sup = super_type.clone().normalize();

        match (sub, sup) {
            (Type::Never, _) => true,
            (_, Type::Any) => true,
            (Type::Any, _) => false,
            (_, Type::Never) => false,
            (Type::Union(items), sup) => items.iter().all(|item| item.is_subtype_of(&sup)),
            (sub, Type::Union(items)) => items.iter().any(|item| sub.is_subtype_of(item)),
            (Type::Array(sub), Type::Array(sup)) => sub.is_subtype_of(&sup),
            (Type::NonEmptyArray(sub), Type::Array(sup)) => sub.is_subtype_of(&sup),
            (Type::NonEmptyArray(sub), Type::NonEmptyArray(sup)) => sub.is_subtype_of(&sup),
            (Type::Tuple(items), Type::Array(sup)) => {
                items.iter().all(|item| item.is_subtype_of(&sup))
            }
            (Type::Tuple(items), Type::NonEmptyArray(sup)) => {
                !items.is_empty() && items.iter().all(|item| item.is_subtype_of(&sup))
            }
            (Type::Tuple(sub), Type::Tuple(sup)) => {
                sub.len() == sup.len()
                    && sub.iter().zip(sup.iter()).all(|(a, b)| a.is_subtype_of(b))
            }
            (Type::Object(sub), Type::Object(sup)) => object_subtype(&sub, &sup),
            (Type::Generic(a), Type::Generic(b)) => a == b,
            (lhs, rhs) => lhs == rhs,
        }
    }

    pub fn from_json_value(value: &Value) -> Type {
        match value {
            Value::Null => Type::Null,
            Value::Bool(_) => Type::Bool,
            Value::Number(_) => Type::Number,
            Value::String(_) => Type::String,
            Value::Array(items) => Type::Tuple(items.iter().map(Type::from_json_value).collect()),
            Value::Object(map) => {
                let fields = map
                    .iter()
                    .map(|(k, v)| (k.clone(), Type::from_json_value(v)))
                    .collect();
                Type::Object(ObjectShape::closed(fields))
            }
        }
    }

    pub fn contains_value(&self, value: &Value) -> bool {
        Type::from_json_value(value).is_subtype_of(self)
    }

    fn stable_key(&self) -> String {
        match self {
            Type::Never => "never".to_string(),
            Type::Any => "any".to_string(),
            Type::Null => "null".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Number => "number".to_string(),
            Type::String => "string".to_string(),
            Type::Array(inner) => format!("array<{}>", inner.stable_key()),
            Type::NonEmptyArray(inner) => format!("nonempty<{}>", inner.stable_key()),
            Type::Tuple(items) => {
                let parts = items
                    .iter()
                    .map(Type::stable_key)
                    .collect::<Vec<_>>()
                    .join(",");
                format!("tuple({parts})")
            }
            Type::Object(shape) => {
                let mut parts = Vec::new();
                for (k, v) in &shape.fields {
                    parts.push(format!("{k}:{}", v.stable_key()));
                }
                format!(
                    "object{{{}|{}}}",
                    parts.join(","),
                    row_tail_stable_key(&shape.tail)
                )
            }
            Type::Union(items) => {
                let mut keys: Vec<String> = items.iter().map(Type::stable_key).collect();
                keys.sort();
                format!("union({})", keys.join("|"))
            }
            Type::Generic(name) => format!("generic<{}>", name),
        }
    }
}

fn normalize_row_tail(tail: RowTail) -> RowTail {
    match tail {
        RowTail::Closed => RowTail::Closed,
        RowTail::Open(inner) => {
            let t = inner.normalize();
            if t.is_never() {
                RowTail::Closed
            } else {
                RowTail::Open(Box::new(t))
            }
        }
        RowTail::Var(name) => RowTail::Var(name),
    }
}

fn row_tail_stable_key(tail: &RowTail) -> String {
    match tail {
        RowTail::Closed => "closed".to_string(),
        RowTail::Open(t) => format!("open:{}", t.stable_key()),
        RowTail::Var(name) => format!("var:{name}"),
    }
}

fn row_tail_accepts_field(tail: &RowTail, field_ty: &Type) -> bool {
    match tail {
        RowTail::Closed => false,
        RowTail::Open(ty) => field_ty.is_subtype_of(ty),
        RowTail::Var(_) => true,
    }
}

fn row_tail_is_subtype(sub: &RowTail, sup: &RowTail) -> bool {
    match (sub, sup) {
        (RowTail::Closed, _) => true,
        (RowTail::Open(_), RowTail::Closed) => false,
        (RowTail::Open(sub), RowTail::Open(sup)) => sub.is_subtype_of(sup),
        (RowTail::Open(_), RowTail::Var(_)) => true,
        (RowTail::Var(a), RowTail::Var(b)) => a == b,
        (RowTail::Var(_), _) => false,
    }
}

fn intersect_row_tail(lhs: RowTail, rhs: RowTail) -> RowTail {
    match (lhs, rhs) {
        (RowTail::Closed, _) | (_, RowTail::Closed) => RowTail::Closed,
        (RowTail::Open(l), RowTail::Open(r)) => {
            let t = l.intersect(&r).normalize();
            if t.is_never() {
                RowTail::Closed
            } else {
                RowTail::Open(Box::new(t))
            }
        }
        (RowTail::Open(l), RowTail::Var(_)) | (RowTail::Var(_), RowTail::Open(l)) => {
            RowTail::Open(l)
        }
        (RowTail::Var(a), RowTail::Var(b)) => {
            if a == b {
                RowTail::Var(a)
            } else {
                RowTail::Var(if a < b { a } else { b })
            }
        }
    }
}

fn subtract_row_tail(lhs: RowTail, rhs: RowTail) -> RowTail {
    match (lhs, rhs) {
        (RowTail::Closed, _) => RowTail::Closed,
        (RowTail::Open(l), RowTail::Closed) => RowTail::Open(l),
        (RowTail::Open(l), RowTail::Open(r)) => {
            let rem = l.subtract(&r).normalize();
            if rem.is_never() {
                RowTail::Closed
            } else {
                RowTail::Open(Box::new(rem))
            }
        }
        (RowTail::Open(_), RowTail::Var(_)) => RowTail::Closed,
        (RowTail::Var(a), RowTail::Var(b)) => {
            if a == b {
                RowTail::Closed
            } else {
                RowTail::Var(a)
            }
        }
        (RowTail::Var(a), _) => RowTail::Var(a),
    }
}

fn normalize_union(items: Vec<Type>) -> Type {
    let mut flattened = Vec::new();
    for item in items {
        match item.normalize() {
            Type::Never => {}
            Type::Any => return Type::Any,
            Type::Union(inner) => flattened.extend(inner),
            other => flattened.push(other),
        }
    }

    let mut by_key = BTreeMap::new();
    for item in flattened {
        by_key.entry(item.stable_key()).or_insert(item);
    }

    let values: Vec<Type> = by_key.into_values().collect();

    let mut pruned = Vec::new();
    for (i, item) in values.iter().enumerate() {
        let subsumed = values
            .iter()
            .enumerate()
            .any(|(j, other)| i != j && item.is_subtype_of(other) && !other.is_subtype_of(item));
        if !subsumed {
            pruned.push(item.clone());
        }
    }

    let mut keyset = BTreeSet::new();
    pruned.retain(|item| keyset.insert(item.stable_key()));
    pruned.sort_by_key(Type::stable_key);

    if pruned.is_empty() {
        Type::Never
    } else if pruned.len() == 1 {
        pruned.pop().unwrap_or(Type::Never)
    } else {
        Type::Union(pruned)
    }
}

fn intersect_array_tuple(array_elem: &Type, tuple_items: &[Type]) -> Type {
    let mut out = Vec::with_capacity(tuple_items.len());
    for item in tuple_items {
        let t = array_elem.intersect(item).normalize();
        if t.is_never() {
            return Type::Never;
        }
        out.push(t);
    }
    Type::Tuple(out)
}

fn object_subtype(sub: &ObjectShape, sup: &ObjectShape) -> bool {
    for (key, super_ty) in &sup.fields {
        let Some(sub_ty) = sub.fields.get(key) else {
            return false;
        };
        if !sub_ty.is_subtype_of(super_ty) {
            return false;
        }
    }

    for (key, sub_ty) in &sub.fields {
        if !sup.fields.contains_key(key) && !row_tail_accepts_field(&sup.tail, sub_ty) {
            return false;
        }
    }

    row_tail_is_subtype(&sub.tail, &sup.tail)
}

fn intersect_object_shape(lhs: ObjectShape, rhs: ObjectShape) -> Type {
    let mut fields = BTreeMap::new();

    let mut keys: BTreeSet<String> = lhs.fields.keys().cloned().collect();
    keys.extend(rhs.fields.keys().cloned());

    for key in keys {
        let lhs_ty = lhs.fields.get(&key).cloned();
        let rhs_ty = rhs.fields.get(&key).cloned();

        let combined = match (lhs_ty, rhs_ty) {
            (Some(l), Some(r)) => l.intersect(&r).normalize(),
            (Some(l), None) => match &rhs.tail {
                RowTail::Closed => return Type::Never,
                RowTail::Open(t) => l.intersect(t).normalize(),
                RowTail::Var(_) => l,
            },
            (None, Some(r)) => match &lhs.tail {
                RowTail::Closed => return Type::Never,
                RowTail::Open(t) => r.intersect(t).normalize(),
                RowTail::Var(_) => r,
            },
            (None, None) => continue,
        };

        if combined.is_never() {
            return Type::Never;
        }
        fields.insert(key, combined);
    }

    let tail = intersect_row_tail(lhs.tail, rhs.tail);
    Type::Object(ObjectShape { fields, tail }).normalize()
}

fn subtract_object_shape(lhs: ObjectShape, rhs: ObjectShape) -> Type {
    let lhs_ty = Type::Object(lhs.clone());
    let rhs_ty = Type::Object(rhs.clone());

    if lhs_ty.is_subtype_of(&rhs_ty) {
        return Type::Never;
    }

    if lhs.fields == rhs.fields && lhs.tail == rhs.tail {
        return Type::Never;
    }

    if lhs.fields == rhs.fields {
        let mut fields = BTreeMap::new();
        for (key, lhs_field) in lhs.fields {
            let rhs_field = rhs.fields.get(&key).cloned().unwrap_or(Type::Never);
            let remaining = lhs_field.subtract(&rhs_field).normalize();
            if remaining.is_never() {
                return Type::Never;
            }
            fields.insert(key, remaining);
        }

        let tail = subtract_row_tail(lhs.tail, rhs.tail);
        return Type::Object(ObjectShape { fields, tail }).normalize();
    }

    Type::Object(lhs)
}

impl fmt::Display for RowTail {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RowTail::Closed => Ok(()),
            RowTail::Open(rest) => write!(f, "..{rest}"),
            RowTail::Var(name) => write!(f, "..{name}"),
        }
    }
}

impl fmt::Display for ObjectShape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut parts = Vec::new();
        for (key, ty) in &self.fields {
            parts.push(format!("{key}: {ty}"));
        }

        match &self.tail {
            RowTail::Closed => {}
            RowTail::Open(rest) => parts.push(format!("..{rest}")),
            RowTail::Var(name) => parts.push(format!("..{name}")),
        }

        write!(f, "{{{}}}", parts.join(", "))
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Never => write!(f, "Never"),
            Type::Any => write!(f, "Any"),
            Type::Null => write!(f, "Null"),
            Type::Bool => write!(f, "Bool"),
            Type::Number => write!(f, "Number"),
            Type::String => write!(f, "String"),
            Type::Array(inner) => write!(f, "Array<{}>", inner),
            Type::NonEmptyArray(inner) => write!(f, "NonEmptyArray<{}>", inner),
            Type::Tuple(items) => {
                let text = items
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "Tuple<{}>", text)
            }
            Type::Object(shape) => {
                if shape.is_open_any() {
                    write!(f, "Object")
                } else {
                    write!(f, "Object{}", shape)
                }
            }
            Type::Union(items) => {
                let text = items
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" | ");
                write!(f, "{}", text)
            }
            Type::Generic(name) => write!(f, "{}", name),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{ObjectShape, RowTail, Type};
    use serde_json::json;
    use std::collections::BTreeMap;

    #[test]
    fn tuple_is_subtype_of_uniform_array() {
        let tuple = Type::Tuple(vec![Type::Number, Type::Number, Type::Number]);
        let array = Type::Array(Box::new(Type::Number));
        assert!(tuple.is_subtype_of(&array));
    }

    #[test]
    fn tuple_is_subtype_of_nonempty_array_when_nonempty() {
        let tuple = Type::Tuple(vec![Type::Number, Type::Number]);
        let nonempty = Type::NonEmptyArray(Box::new(Type::Number));
        assert!(tuple.is_subtype_of(&nonempty));
        assert!(!Type::Tuple(vec![]).is_subtype_of(&nonempty));
    }

    #[test]
    fn object_shape_tracks_required_keys() {
        let mut fields = BTreeMap::new();
        fields.insert("x".to_string(), Type::Number);
        let shape = Type::Object(ObjectShape {
            fields,
            tail: RowTail::Open(Box::new(Type::Any)),
        });

        assert!(Type::from_json_value(&json!({"x": 3})).is_subtype_of(&shape));
        assert!(!Type::from_json_value(&json!({"y": 3})).is_subtype_of(&shape));
    }

    #[test]
    fn row_tail_variable_accepts_extra_fields() {
        let shape = Type::Object(ObjectShape::with_required_field_row(
            "x",
            Type::Number,
            "R0",
        ));
        let value = Type::from_json_value(&json!({"x": 1, "y": "extra"}));
        assert!(value.is_subtype_of(&shape));
    }

    #[test]
    fn subtract_number_from_any() {
        let result = Type::Any.subtract(&Type::Number);
        assert_eq!(
            result,
            Type::union(vec![
                Type::Null,
                Type::Bool,
                Type::String,
                Type::Array(Box::new(Type::Any)),
                Type::any_object(),
            ])
        );
    }

    #[test]
    fn intersect_object_requirements_merges_keys() {
        let left = Type::Object(ObjectShape::with_required_field("a", Type::Number));
        let right = Type::Object(ObjectShape::with_required_field("b", Type::String));

        let merged = left.intersect(&right);
        let expected = Type::Object(ObjectShape {
            fields: [
                ("a".to_string(), Type::Number),
                ("b".to_string(), Type::String),
            ]
            .into_iter()
            .collect(),
            tail: RowTail::Open(Box::new(Type::Any)),
        });

        assert_eq!(merged, expected);
    }

    #[test]
    fn from_json_array_becomes_tuple() {
        let ty = Type::from_json_value(&json!([1, "x", true]));
        assert_eq!(
            ty,
            Type::Tuple(vec![Type::Number, Type::String, Type::Bool])
        );
    }

    #[test]
    fn subtract_empty_tuple_from_array_yields_nonempty_array() {
        let ty = Type::Array(Box::new(Type::Any));
        let out = ty.subtract(&Type::Tuple(vec![]));
        assert_eq!(out, Type::NonEmptyArray(Box::new(Type::Any)));
    }
}
