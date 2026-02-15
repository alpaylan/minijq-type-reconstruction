use crate::minijq::typing::Builtin;
use serde_json::Value;
use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
    Alt,
}

impl BinaryOp {
    pub fn symbol(&self) -> &'static str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Eq => "==",
            BinaryOp::Ne => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Lte => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Gte => ">=",
            BinaryOp::And => "and",
            BinaryOp::Or => "or",
            BinaryOp::Alt => "//",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Neg,
}

impl UnaryOp {
    pub fn symbol(&self) -> &'static str {
        match self {
            UnaryOp::Not => "not",
            UnaryOp::Neg => "-",
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Identity,
    Literal(Value),
    Pipe(Box<Expr>, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Builtin(Builtin, Box<Expr>),
    Field(Box<Expr>, String),
    Index(Box<Expr>, usize),
    Lookup(Box<Expr>, Box<Expr>),
    Optional(Box<Expr>),
    TryCatch {
        try_expr: Box<Expr>,
        catch_expr: Box<Expr>,
    },
    IfElse {
        cond: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    ArrayLiteral(Vec<Expr>),
    ObjectLiteral(Vec<(String, Expr)>),
}

impl Expr {
    pub fn identity() -> Expr {
        Expr::Identity
    }

    pub fn literal(value: Value) -> Expr {
        Expr::Literal(value)
    }

    pub fn pipe(lhs: Expr, rhs: Expr) -> Expr {
        Expr::Pipe(Box::new(lhs), Box::new(rhs))
    }

    pub fn binary(op: BinaryOp, lhs: Expr, rhs: Expr) -> Expr {
        Expr::Binary(op, Box::new(lhs), Box::new(rhs))
    }

    pub fn unary(op: UnaryOp, inner: Expr) -> Expr {
        Expr::Unary(op, Box::new(inner))
    }

    pub fn add(lhs: Expr, rhs: Expr) -> Expr {
        Expr::binary(BinaryOp::Add, lhs, rhs)
    }

    pub fn sub(lhs: Expr, rhs: Expr) -> Expr {
        Expr::binary(BinaryOp::Sub, lhs, rhs)
    }

    pub fn mul(lhs: Expr, rhs: Expr) -> Expr {
        Expr::binary(BinaryOp::Mul, lhs, rhs)
    }

    pub fn div(lhs: Expr, rhs: Expr) -> Expr {
        Expr::binary(BinaryOp::Div, lhs, rhs)
    }

    pub fn eq(lhs: Expr, rhs: Expr) -> Expr {
        Expr::binary(BinaryOp::Eq, lhs, rhs)
    }

    pub fn ne(lhs: Expr, rhs: Expr) -> Expr {
        Expr::binary(BinaryOp::Ne, lhs, rhs)
    }

    pub fn lt(lhs: Expr, rhs: Expr) -> Expr {
        Expr::binary(BinaryOp::Lt, lhs, rhs)
    }

    pub fn lte(lhs: Expr, rhs: Expr) -> Expr {
        Expr::binary(BinaryOp::Lte, lhs, rhs)
    }

    pub fn gt(lhs: Expr, rhs: Expr) -> Expr {
        Expr::binary(BinaryOp::Gt, lhs, rhs)
    }

    pub fn gte(lhs: Expr, rhs: Expr) -> Expr {
        Expr::binary(BinaryOp::Gte, lhs, rhs)
    }

    pub fn and(lhs: Expr, rhs: Expr) -> Expr {
        Expr::binary(BinaryOp::And, lhs, rhs)
    }

    pub fn or(lhs: Expr, rhs: Expr) -> Expr {
        Expr::binary(BinaryOp::Or, lhs, rhs)
    }

    pub fn alt(lhs: Expr, rhs: Expr) -> Expr {
        Expr::binary(BinaryOp::Alt, lhs, rhs)
    }

    pub fn not(inner: Expr) -> Expr {
        Expr::unary(UnaryOp::Not, inner)
    }

    pub fn neg(inner: Expr) -> Expr {
        Expr::unary(UnaryOp::Neg, inner)
    }

    pub fn builtin(op: Builtin, input: Expr) -> Expr {
        Expr::Builtin(op, Box::new(input))
    }

    pub fn field(input: Expr, name: impl Into<String>) -> Expr {
        Expr::Field(Box::new(input), name.into())
    }

    pub fn index(input: Expr, index: usize) -> Expr {
        Expr::Index(Box::new(input), index)
    }

    pub fn lookup(input: Expr, key: Expr) -> Expr {
        Expr::Lookup(Box::new(input), Box::new(key))
    }

    pub fn optional(inner: Expr) -> Expr {
        Expr::Optional(Box::new(inner))
    }

    pub fn try_catch(try_expr: Expr, catch_expr: Expr) -> Expr {
        Expr::TryCatch {
            try_expr: Box::new(try_expr),
            catch_expr: Box::new(catch_expr),
        }
    }

    pub fn if_else(cond: Expr, then_branch: Expr, else_branch: Expr) -> Expr {
        Expr::IfElse {
            cond: Box::new(cond),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        }
    }

    pub fn array(items: Vec<Expr>) -> Expr {
        Expr::ArrayLiteral(items)
    }

    pub fn object(fields: Vec<(String, Expr)>) -> Expr {
        Expr::ObjectLiteral(fields)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Identity => write!(f, "."),
            Expr::Literal(value) => write!(f, "{}", value),
            Expr::Pipe(lhs, rhs) => write!(f, "{} | {}", lhs, rhs),
            Expr::Binary(op, lhs, rhs) => write!(f, "({} {} {})", lhs, op.symbol(), rhs),
            Expr::Unary(UnaryOp::Not, inner) => write!(f, "(not {})", inner),
            Expr::Unary(UnaryOp::Neg, inner) => write!(f, "(-{})", inner),
            Expr::Builtin(op, expr) => write!(f, "{}({})", op.name(), expr),
            Expr::Field(expr, name) => {
                if matches!(expr.as_ref(), Expr::Identity) {
                    write!(f, ".{}", name)
                } else {
                    write!(f, "{}.{}", expr, name)
                }
            }
            Expr::Index(expr, index) => {
                if matches!(expr.as_ref(), Expr::Identity) {
                    write!(f, ".[{}]", index)
                } else {
                    write!(f, "{}[{}]", expr, index)
                }
            }
            Expr::Lookup(expr, key) => {
                if matches!(expr.as_ref(), Expr::Identity) {
                    write!(f, ".[{}]", key)
                } else {
                    write!(f, "{}[{}]", expr, key)
                }
            }
            Expr::Optional(inner) => write!(f, "{}?", inner),
            Expr::TryCatch {
                try_expr,
                catch_expr,
            } => write!(f, "try {} catch {}", try_expr, catch_expr),
            Expr::IfElse {
                cond,
                then_branch,
                else_branch,
            } => write!(
                f,
                "if {} then {} else {} end",
                cond, then_branch, else_branch
            ),
            Expr::ArrayLiteral(items) => {
                let text = items
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "[{}]", text)
            }
            Expr::ObjectLiteral(fields) => {
                let text = fields
                    .iter()
                    .map(|(k, v)| {
                        let key = serde_json::to_string(k)
                            .unwrap_or_else(|_| format!("\"{}\"", k.replace('"', "\\\"")));
                        format!("{key}: {v}")
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{{{}}}", text)
            }
        }
    }
}
