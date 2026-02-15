use crate::minijq::types::{ObjectShape, RowTail, Type};
use rand::prelude::SliceRandom;
use rand::Rng;
use serde_json::{Map, Number, Value};

pub fn generate_value(ty: &Type, rng: &mut impl Rng, max_depth: usize) -> Option<Value> {
    let t = ty.clone().normalize();
    match t {
        Type::Never => None,
        Type::Any => {
            let mut choices = vec![Type::Null, Type::Bool, Type::Number, Type::String];
            if max_depth > 0 {
                choices.push(Type::Array(Box::new(Type::Any)));
                choices.push(Type::Object(ObjectShape::open_any()));
            }
            let choice = choices.choose(rng)?.clone();
            generate_value(&choice, rng, max_depth)
        }
        Type::Null => Some(Value::Null),
        Type::Bool => Some(Value::Bool(rng.gen_bool(0.5))),
        Type::Number => {
            let value: i64 = rng.gen_range(-100..=100);
            Some(Value::Number(Number::from(value)))
        }
        Type::String => {
            let len = rng.gen_range(0..=8);
            let mut s = String::with_capacity(len);
            for _ in 0..len {
                let ch = (b'a' + rng.gen_range(0..26)) as char;
                s.push(ch);
            }
            Some(Value::String(s))
        }
        Type::Array(inner) => {
            if max_depth == 0 {
                return Some(Value::Array(Vec::new()));
            }

            let len = if inner.is_never() {
                0
            } else {
                rng.gen_range(0..=3)
            };
            let mut items = Vec::with_capacity(len);
            for _ in 0..len {
                let value = generate_value(&inner, rng, max_depth.saturating_sub(1))?;
                items.push(value);
            }
            Some(Value::Array(items))
        }
        Type::NonEmptyArray(inner) => {
            if max_depth == 0 {
                let item = generate_value(&inner, rng, 0)?;
                return Some(Value::Array(vec![item]));
            }

            let len = if inner.is_never() {
                1
            } else {
                rng.gen_range(1..=3)
            };
            let mut items = Vec::with_capacity(len);
            for _ in 0..len {
                let value = generate_value(&inner, rng, max_depth.saturating_sub(1))?;
                items.push(value);
            }
            Some(Value::Array(items))
        }
        Type::Tuple(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(generate_value(&item, rng, max_depth.saturating_sub(1))?);
            }
            Some(Value::Array(out))
        }
        Type::Object(shape) => generate_object(shape, rng, max_depth),
        Type::Union(items) => {
            let choices: Vec<Type> = items.into_iter().filter(|item| !item.is_never()).collect();
            let choice = choices.choose(rng)?.clone();
            generate_value(&choice, rng, max_depth)
        }
        Type::Generic(_) => generate_value(&Type::Any, rng, max_depth),
    }
}

fn generate_object(shape: ObjectShape, rng: &mut impl Rng, max_depth: usize) -> Option<Value> {
    let mut map = Map::new();

    for (key, field_ty) in &shape.fields {
        map.insert(
            key.clone(),
            generate_value(field_ty, rng, max_depth.saturating_sub(1))?,
        );
    }

    if max_depth > 0 {
        match shape.tail {
            RowTail::Closed => {}
            RowTail::Open(rest) => {
                let extra_count = rng.gen_range(0..=2);
                for i in 0..extra_count {
                    let key = format!("extra_{i}");
                    if map.contains_key(&key) {
                        continue;
                    }
                    map.insert(
                        key,
                        generate_value(&rest, rng, max_depth.saturating_sub(1))?,
                    );
                }
            }
            RowTail::Var(_) => {
                let extra_count = rng.gen_range(0..=2);
                for i in 0..extra_count {
                    let key = format!("extra_{i}");
                    if map.contains_key(&key) {
                        continue;
                    }
                    map.insert(
                        key,
                        generate_value(&Type::Any, rng, max_depth.saturating_sub(1))?,
                    );
                }
            }
        }
    }

    Some(Value::Object(map))
}

#[cfg(test)]
mod tests {
    use super::generate_value;
    use crate::minijq::types::{ObjectShape, Type};
    use rand::rngs::StdRng;
    use rand::SeedableRng;

    #[test]
    fn never_generates_nothing() {
        let mut rng = StdRng::seed_from_u64(7);
        assert_eq!(generate_value(&Type::Never, &mut rng, 3), None);
    }

    #[test]
    fn generated_values_fit_type() {
        let ty = Type::Tuple(vec![
            Type::Number,
            Type::Object(ObjectShape::with_required_field("x", Type::String)),
        ]);
        let mut rng = StdRng::seed_from_u64(11);

        for _ in 0..200 {
            let value = generate_value(&ty, &mut rng, 4).expect("value should generate");
            assert!(ty.contains_value(&value), "generated value {value} does not match {ty}");
        }
    }

    #[test]
    fn nonempty_array_generates_nonempty() {
        let mut rng = StdRng::seed_from_u64(99);
        let ty = Type::NonEmptyArray(Box::new(Type::Number));

        for _ in 0..200 {
            let value = generate_value(&ty, &mut rng, 3).expect("value should generate");
            let arr = value.as_array().expect("must be array");
            assert!(!arr.is_empty());
        }
    }

    #[test]
    fn any_generates_multiple_shapes() {
        let mut rng = StdRng::seed_from_u64(23);
        let mut saw_array = false;
        let mut saw_object = false;
        let mut saw_primitive = false;

        for _ in 0..200 {
            let value = generate_value(&Type::Any, &mut rng, 3).expect("value should generate");
            match value {
                serde_json::Value::Array(_) => saw_array = true,
                serde_json::Value::Object(_) => saw_object = true,
                _ => saw_primitive = true,
            }
        }

        assert!(saw_array);
        assert!(saw_object);
        assert!(saw_primitive);
    }
}
