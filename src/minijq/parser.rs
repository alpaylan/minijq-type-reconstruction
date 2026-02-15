use crate::minijq::ast::{BinaryOp, Expr, UnaryOp};
use crate::minijq::typing::Builtin;
use serde_json::{Number, Value};
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParseError {
    pub position: usize,
    pub message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parse error at {}: {}", self.position, self.message)
    }
}

impl std::error::Error for ParseError {}

#[derive(Clone, Debug, PartialEq)]
enum Token {
    Dot,
    Pipe,
    SlashSlash,
    Plus,
    Minus,
    Star,
    Slash,
    EqEq,
    NotEq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
    Not,
    If,
    Try,
    Catch,
    Then,
    Else,
    End,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Colon,
    Comma,
    Question,
    Identifier(String),
    Int(i64),
    Float(f64),
    String(String),
    True,
    False,
    Null,
    Eof,
}

#[derive(Clone, Debug)]
struct TokenWithPos {
    token: Token,
    pos: usize,
}

pub fn parse_expr(source: &str) -> Result<Expr, ParseError> {
    let tokens = tokenize(source)?;
    let mut parser = Parser { tokens, index: 0 };
    let expr = parser.parse_pipe()?;
    parser.expect(Token::Eof)?;
    Ok(expr)
}

struct Parser {
    tokens: Vec<TokenWithPos>,
    index: usize,
}

impl Parser {
    fn parse_pipe(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_alt()?;
        while self.match_token(&Token::Pipe) {
            let rhs = self.parse_alt()?;
            expr = Expr::pipe(expr, rhs);
        }
        Ok(expr)
    }

    fn parse_alt(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_or()?;
        while self.match_token(&Token::SlashSlash) {
            let rhs = self.parse_or()?;
            expr = Expr::binary(BinaryOp::Alt, expr, rhs);
        }
        Ok(expr)
    }

    fn parse_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_and()?;
        while self.match_token(&Token::Or) {
            let rhs = self.parse_and()?;
            expr = Expr::binary(BinaryOp::Or, expr, rhs);
        }
        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_compare()?;
        while self.match_token(&Token::And) {
            let rhs = self.parse_compare()?;
            expr = Expr::binary(BinaryOp::And, expr, rhs);
        }
        Ok(expr)
    }

    fn parse_compare(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_add()?;
        loop {
            let op = if self.match_token(&Token::EqEq) {
                Some(BinaryOp::Eq)
            } else if self.match_token(&Token::NotEq) {
                Some(BinaryOp::Ne)
            } else if self.match_token(&Token::Lt) {
                Some(BinaryOp::Lt)
            } else if self.match_token(&Token::Lte) {
                Some(BinaryOp::Lte)
            } else if self.match_token(&Token::Gt) {
                Some(BinaryOp::Gt)
            } else if self.match_token(&Token::Gte) {
                Some(BinaryOp::Gte)
            } else {
                None
            };

            let Some(op) = op else {
                break;
            };
            let rhs = self.parse_add()?;
            expr = Expr::binary(op, expr, rhs);
        }
        Ok(expr)
    }

    fn parse_add(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_mul()?;
        loop {
            let op = if self.match_token(&Token::Plus) {
                Some(BinaryOp::Add)
            } else if self.match_token(&Token::Minus) {
                Some(BinaryOp::Sub)
            } else {
                None
            };

            let Some(op) = op else {
                break;
            };
            let rhs = self.parse_mul()?;
            expr = Expr::binary(op, expr, rhs);
        }
        Ok(expr)
    }

    fn parse_mul(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary()?;
        loop {
            let op = if self.match_token(&Token::Star) {
                Some(BinaryOp::Mul)
            } else if self.match_token(&Token::Slash) {
                Some(BinaryOp::Div)
            } else {
                None
            };

            let Some(op) = op else {
                break;
            };
            let rhs = self.parse_unary()?;
            expr = Expr::binary(op, expr, rhs);
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(&Token::Not) {
            let inner = self.parse_unary()?;
            return Ok(Expr::unary(UnaryOp::Not, inner));
        }

        if self.match_token(&Token::Minus) {
            let inner = self.parse_unary()?;
            return Ok(Expr::unary(UnaryOp::Neg, inner));
        }

        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.match_token(&Token::Dot) {
                let name = self.expect_identifier()?;
                expr = Expr::field(expr, name);
                continue;
            }

            if self.match_token(&Token::LBracket) {
                let key_expr = self.parse_pipe()?;
                self.expect(Token::RBracket)?;
                if let Some(idx) = literal_to_non_negative_index(&key_expr) {
                    expr = Expr::index(expr, idx);
                } else {
                    expr = Expr::lookup(expr, key_expr);
                }
                continue;
            }

            if self.match_token(&Token::Question) {
                expr = Expr::optional(expr);
                continue;
            }

            break;
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(&Token::Try) {
            let try_expr = self.parse_pipe()?;
            self.expect(Token::Catch)?;
            let catch_expr = self.parse_pipe()?;
            return Ok(Expr::try_catch(try_expr, catch_expr));
        }

        if self.match_token(&Token::Dot) {
            let mut expr = Expr::identity();
            loop {
                if let Some(TokenWithPos {
                    token: Token::Identifier(name),
                    ..
                }) = self.peek().cloned()
                {
                    self.advance();
                    expr = Expr::field(expr, name);
                    continue;
                }

                if self.match_token(&Token::LBracket) {
                    let key_expr = self.parse_pipe()?;
                    self.expect(Token::RBracket)?;
                    if let Some(idx) = literal_to_non_negative_index(&key_expr) {
                        expr = Expr::index(expr, idx);
                    } else {
                        expr = Expr::lookup(expr, key_expr);
                    }
                    continue;
                }

                break;
            }
            return Ok(expr);
        }

        if self.match_token(&Token::If) {
            let cond = self.parse_pipe()?;
            self.expect(Token::Then)?;
            let then_branch = self.parse_pipe()?;
            self.expect(Token::Else)?;
            let else_branch = self.parse_pipe()?;
            self.expect(Token::End)?;
            return Ok(Expr::if_else(cond, then_branch, else_branch));
        }

        if self.match_token(&Token::LParen) {
            let expr = self.parse_pipe()?;
            self.expect(Token::RParen)?;
            return Ok(expr);
        }

        if self.match_token(&Token::LBracket) {
            let mut items = Vec::new();
            if !self.peek_is(&Token::RBracket) {
                loop {
                    items.push(self.parse_pipe()?);
                    if self.match_token(&Token::Comma) {
                        continue;
                    }
                    break;
                }
            }
            self.expect(Token::RBracket)?;
            return Ok(Expr::array(items));
        }

        if self.match_token(&Token::LBrace) {
            let mut fields = Vec::new();
            if !self.peek_is(&Token::RBrace) {
                loop {
                    let key = self.expect_object_key()?;
                    self.expect(Token::Colon)?;
                    let value = self.parse_pipe()?;
                    fields.push((key, value));
                    if self.match_token(&Token::Comma) {
                        continue;
                    }
                    break;
                }
            }
            self.expect(Token::RBrace)?;
            return Ok(Expr::object(fields));
        }

        if let Some(TokenWithPos {
            token: Token::Identifier(name),
            ..
        }) = self.peek().cloned()
        {
            self.advance();

            if self.match_token(&Token::LParen) {
                let arg = self.parse_pipe()?;
                self.expect(Token::RParen)?;
                if let Some(builtin) = parse_builtin(&name) {
                    return Ok(Expr::builtin(builtin, arg));
                }
                return Ok(Expr::call(name, arg));
            }

            if let Some(builtin) = parse_builtin(&name) {
                return Ok(Expr::builtin(builtin, Expr::identity()));
            }
            return Ok(Expr::call(name, Expr::identity()));
        }

        if let Some(TokenWithPos {
            token: Token::Int(i),
            ..
        }) = self.peek().cloned()
        {
            self.advance();
            return Ok(Expr::literal(Value::Number(Number::from(i))));
        }

        if let Some(TokenWithPos {
            token: Token::Float(n),
            pos,
        }) = self.peek().cloned()
        {
            self.advance();
            let number = Number::from_f64(n).ok_or(ParseError {
                position: pos,
                message: "invalid numeric literal".to_string(),
            })?;
            return Ok(Expr::literal(Value::Number(number)));
        }

        if let Some(TokenWithPos {
            token: Token::String(s),
            ..
        }) = self.peek().cloned()
        {
            self.advance();
            return Ok(Expr::literal(Value::String(s)));
        }

        if self.match_token(&Token::True) {
            return Ok(Expr::literal(Value::Bool(true)));
        }

        if self.match_token(&Token::False) {
            return Ok(Expr::literal(Value::Bool(false)));
        }

        if self.match_token(&Token::Null) {
            return Ok(Expr::literal(Value::Null));
        }

        let pos = self.peek().map(|t| t.pos).unwrap_or(0);
        Err(ParseError {
            position: pos,
            message: "unexpected token".to_string(),
        })
    }

    fn expect_object_key(&mut self) -> Result<String, ParseError> {
        match self.peek().cloned() {
            Some(TokenWithPos {
                token: Token::Identifier(name),
                ..
            }) => {
                self.advance();
                Ok(name)
            }
            Some(TokenWithPos {
                token: Token::String(name),
                ..
            }) => {
                self.advance();
                Ok(name)
            }
            Some(tok) => Err(ParseError {
                position: tok.pos,
                message: "expected object key".to_string(),
            }),
            None => Err(ParseError {
                position: 0,
                message: "expected object key".to_string(),
            }),
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        match self.peek().cloned() {
            Some(TokenWithPos {
                token: Token::Identifier(name),
                ..
            }) => {
                self.advance();
                Ok(name)
            }
            Some(tok) => Err(ParseError {
                position: tok.pos,
                message: "expected identifier".to_string(),
            }),
            None => Err(ParseError {
                position: 0,
                message: "expected identifier".to_string(),
            }),
        }
    }

    fn expect(&mut self, expected: Token) -> Result<(), ParseError> {
        match self.peek().cloned() {
            Some(tok) if tok.token == expected => {
                self.advance();
                Ok(())
            }
            Some(tok) => Err(ParseError {
                position: tok.pos,
                message: format!("expected {:?}", expected),
            }),
            None => Err(ParseError {
                position: 0,
                message: format!("expected {:?}", expected),
            }),
        }
    }

    fn match_token(&mut self, token: &Token) -> bool {
        if self.peek().is_some_and(|next| &next.token == token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn peek_is(&self, token: &Token) -> bool {
        self.peek().is_some_and(|next| &next.token == token)
    }

    fn peek(&self) -> Option<&TokenWithPos> {
        self.tokens.get(self.index)
    }

    fn advance(&mut self) {
        self.index = self.index.saturating_add(1);
    }
}

fn parse_builtin(name: &str) -> Option<Builtin> {
    match name {
        "error" => Some(Builtin::Error),
        "length" => Some(Builtin::Length),
        "first" => Some(Builtin::First),
        "last" => Some(Builtin::Last),
        "keys" => Some(Builtin::Keys),
        "type" => Some(Builtin::TypeName),
        "map" => Some(Builtin::Map),
        "select" => Some(Builtin::Select),
        "has" => Some(Builtin::Has),
        "contains" => Some(Builtin::Contains),
        "startswith" => Some(Builtin::StartsWith),
        "endswith" => Some(Builtin::EndsWith),
        "reverse" => Some(Builtin::Reverse),
        "sort" => Some(Builtin::Sort),
        "tostring" => Some(Builtin::ToString),
        "tonumber" => Some(Builtin::ToNumber),
        "abs" => Some(Builtin::Abs),
        "floor" => Some(Builtin::Floor),
        "ceil" => Some(Builtin::Ceil),
        "add" => Some(Builtin::Add),
        "min" => Some(Builtin::Min),
        "max" => Some(Builtin::Max),
        "split" => Some(Builtin::Split),
        "join" => Some(Builtin::Join),
        _ => None,
    }
}

fn tokenize(input: &str) -> Result<Vec<TokenWithPos>, ParseError> {
    let mut tokens = Vec::new();
    let bytes = input.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        let b = bytes[i];
        if b.is_ascii_whitespace() {
            i += 1;
            continue;
        }

        match b {
            b'.' => {
                tokens.push(TokenWithPos {
                    token: Token::Dot,
                    pos: i,
                });
                i += 1;
            }
            b'|' => {
                tokens.push(TokenWithPos {
                    token: Token::Pipe,
                    pos: i,
                });
                i += 1;
            }
            b'/' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'/' {
                    tokens.push(TokenWithPos {
                        token: Token::SlashSlash,
                        pos: i,
                    });
                    i += 2;
                } else {
                    tokens.push(TokenWithPos {
                        token: Token::Slash,
                        pos: i,
                    });
                    i += 1;
                }
            }
            b'+' => {
                tokens.push(TokenWithPos {
                    token: Token::Plus,
                    pos: i,
                });
                i += 1;
            }
            b'-' => {
                tokens.push(TokenWithPos {
                    token: Token::Minus,
                    pos: i,
                });
                i += 1;
            }
            b'*' => {
                tokens.push(TokenWithPos {
                    token: Token::Star,
                    pos: i,
                });
                i += 1;
            }
            b'=' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    tokens.push(TokenWithPos {
                        token: Token::EqEq,
                        pos: i,
                    });
                    i += 2;
                } else {
                    return Err(ParseError {
                        position: i,
                        message: "expected `==`".to_string(),
                    });
                }
            }
            b'!' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    tokens.push(TokenWithPos {
                        token: Token::NotEq,
                        pos: i,
                    });
                    i += 2;
                } else {
                    return Err(ParseError {
                        position: i,
                        message: "expected `!=`".to_string(),
                    });
                }
            }
            b'<' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    tokens.push(TokenWithPos {
                        token: Token::Lte,
                        pos: i,
                    });
                    i += 2;
                } else {
                    tokens.push(TokenWithPos {
                        token: Token::Lt,
                        pos: i,
                    });
                    i += 1;
                }
            }
            b'>' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    tokens.push(TokenWithPos {
                        token: Token::Gte,
                        pos: i,
                    });
                    i += 2;
                } else {
                    tokens.push(TokenWithPos {
                        token: Token::Gt,
                        pos: i,
                    });
                    i += 1;
                }
            }
            b'(' => {
                tokens.push(TokenWithPos {
                    token: Token::LParen,
                    pos: i,
                });
                i += 1;
            }
            b')' => {
                tokens.push(TokenWithPos {
                    token: Token::RParen,
                    pos: i,
                });
                i += 1;
            }
            b'[' => {
                tokens.push(TokenWithPos {
                    token: Token::LBracket,
                    pos: i,
                });
                i += 1;
            }
            b']' => {
                tokens.push(TokenWithPos {
                    token: Token::RBracket,
                    pos: i,
                });
                i += 1;
            }
            b'{' => {
                tokens.push(TokenWithPos {
                    token: Token::LBrace,
                    pos: i,
                });
                i += 1;
            }
            b'}' => {
                tokens.push(TokenWithPos {
                    token: Token::RBrace,
                    pos: i,
                });
                i += 1;
            }
            b':' => {
                tokens.push(TokenWithPos {
                    token: Token::Colon,
                    pos: i,
                });
                i += 1;
            }
            b',' => {
                tokens.push(TokenWithPos {
                    token: Token::Comma,
                    pos: i,
                });
                i += 1;
            }
            b'?' => {
                tokens.push(TokenWithPos {
                    token: Token::Question,
                    pos: i,
                });
                i += 1;
            }
            b'"' => {
                let start = i;
                i += 1;
                let mut out = String::new();
                let mut terminated = false;
                while i < bytes.len() {
                    match bytes[i] {
                        b'"' => {
                            i += 1;
                            terminated = true;
                            break;
                        }
                        b'\\' => {
                            i += 1;
                            if i >= bytes.len() {
                                return Err(ParseError {
                                    position: start,
                                    message: "unterminated escape".to_string(),
                                });
                            }
                            let escaped = match bytes[i] {
                                b'"' => '"',
                                b'\\' => '\\',
                                b'n' => '\n',
                                b'r' => '\r',
                                b't' => '\t',
                                other => {
                                    return Err(ParseError {
                                        position: i,
                                        message: format!(
                                            "unsupported string escape \\{}",
                                            other as char
                                        ),
                                    });
                                }
                            };
                            out.push(escaped);
                            i += 1;
                        }
                        other => {
                            out.push(other as char);
                            i += 1;
                        }
                    }
                }

                if !terminated {
                    return Err(ParseError {
                        position: start,
                        message: "unterminated string".to_string(),
                    });
                }

                tokens.push(TokenWithPos {
                    token: Token::String(out),
                    pos: start,
                });
            }
            b'0'..=b'9' => {
                let start = i;
                while i < bytes.len() && bytes[i].is_ascii_digit() {
                    i += 1;
                }

                if i + 1 < bytes.len() && bytes[i] == b'.' && bytes[i + 1].is_ascii_digit() {
                    i += 1;
                    while i < bytes.len() && bytes[i].is_ascii_digit() {
                        i += 1;
                    }

                    let text = &input[start..i];
                    let value = text.parse::<f64>().map_err(|_| ParseError {
                        position: start,
                        message: "invalid float literal".to_string(),
                    })?;
                    tokens.push(TokenWithPos {
                        token: Token::Float(value),
                        pos: start,
                    });
                } else {
                    let text = &input[start..i];
                    let value = text.parse::<i64>().map_err(|_| ParseError {
                        position: start,
                        message: "invalid integer literal".to_string(),
                    })?;
                    tokens.push(TokenWithPos {
                        token: Token::Int(value),
                        pos: start,
                    });
                }
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let start = i;
                i += 1;
                while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
                    i += 1;
                }
                let ident = &input[start..i];
                let token = match ident {
                    "and" => Token::And,
                    "or" => Token::Or,
                    "not" => Token::Not,
                    "if" => Token::If,
                    "try" => Token::Try,
                    "catch" => Token::Catch,
                    "then" => Token::Then,
                    "else" => Token::Else,
                    "end" => Token::End,
                    "true" => Token::True,
                    "false" => Token::False,
                    "null" => Token::Null,
                    _ => Token::Identifier(ident.to_string()),
                };
                tokens.push(TokenWithPos { token, pos: start });
            }
            _ => {
                return Err(ParseError {
                    position: i,
                    message: format!("unexpected character `{}`", bytes[i] as char),
                });
            }
        }
    }

    tokens.push(TokenWithPos {
        token: Token::Eof,
        pos: input.len(),
    });
    Ok(tokens)
}

fn literal_to_non_negative_index(expr: &Expr) -> Option<usize> {
    let Expr::Literal(value) = expr else {
        return None;
    };
    let n = value.as_i64()?;
    if n < 0 {
        return None;
    }
    usize::try_from(n).ok()
}

#[cfg(test)]
mod tests {
    use super::parse_expr;

    #[test]
    fn parse_identity() {
        let expr = parse_expr(".").expect("must parse");
        assert_eq!(expr.to_string(), ".");
    }

    #[test]
    fn parse_field_and_index_chain() {
        let expr = parse_expr(".users[0].name").expect("must parse");
        assert_eq!(expr.to_string(), ".users[0].name");
    }

    #[test]
    fn parse_arithmetic_precedence() {
        let expr = parse_expr(".a + 1 * 2").expect("must parse");
        assert_eq!(expr.to_string(), "(.a + (1 * 2))");
    }

    #[test]
    fn parse_comparison_and_logic() {
        let expr = parse_expr(".x >= 10 and .ok").expect("must parse");
        assert_eq!(expr.to_string(), "((.x >= 10) and .ok)");
    }

    #[test]
    fn parse_if_then_else() {
        let expr = parse_expr("if .x > 0 then .x else 0 end").expect("must parse");
        assert_eq!(expr.to_string(), "if (.x > 0) then .x else 0 end");
    }

    #[test]
    fn parse_array_and_object_literals() {
        let expr = parse_expr("{sum: (.a + .b), items: [.a, .b, 3]}").expect("must parse");
        assert_eq!(
            expr.to_string(),
            "{\"sum\": (.a + .b), \"items\": [.a, .b, 3]}"
        );
    }

    #[test]
    fn parse_alt_and_pipe() {
        let expr = parse_expr(".name // \"n/a\" | length(.)").expect("must parse");
        assert_eq!(expr.to_string(), "(.name // \"n/a\") | length(.)");
    }

    #[test]
    fn parse_map_select() {
        let expr = parse_expr("map(select(. > 0))").expect("must parse");
        assert_eq!(expr.to_string(), "map(select((. > 0)))");
    }

    #[test]
    fn parse_user_filter_reference() {
        let expr = parse_expr("isboolean").expect("must parse");
        assert_eq!(expr.to_string(), "isboolean");
    }

    #[test]
    fn parse_user_filter_call_with_argument() {
        let expr = parse_expr("isboolean(.x)").expect("must parse");
        assert_eq!(expr.to_string(), "isboolean(.x)");
    }

    #[test]
    fn parse_float_literal() {
        let expr = parse_expr(".x + 1.25").expect("must parse");
        assert_eq!(expr.to_string(), "(.x + 1.25)");
    }

    #[test]
    fn parse_extended_builtins() {
        let expr = parse_expr("startswith(\"ab\") and has(\"id\")").expect("must parse");
        assert_eq!(expr.to_string(), "(startswith(\"ab\") and has(\"id\"))");
    }

    #[test]
    fn parse_sort_and_reverse() {
        let expr = parse_expr("sort(.) | reverse(.)").expect("must parse");
        assert_eq!(expr.to_string(), "sort(.) | reverse(.)");
    }

    #[test]
    fn parse_dynamic_lookup() {
        let expr = parse_expr(".data[.key]").expect("must parse");
        assert_eq!(expr.to_string(), ".data[.key]");
    }

    #[test]
    fn parse_optional_postfix() {
        let expr = parse_expr(".user.age? // 0").expect("must parse");
        assert_eq!(expr.to_string(), "(.user.age? // 0)");
    }

    #[test]
    fn parse_conversion_builtins() {
        let expr = parse_expr("tonumber(.) | abs(.) | floor(.)").expect("must parse");
        assert_eq!(expr.to_string(), "tonumber(.) | abs(.) | floor(.)");
    }

    #[test]
    fn parse_try_catch() {
        let expr = parse_expr("try .x catch 0").expect("must parse");
        assert_eq!(expr.to_string(), "try .x catch 0");
    }

    #[test]
    fn parse_aggregate_builtins() {
        let expr = parse_expr("add(.) + max(.)").expect("must parse");
        assert_eq!(expr.to_string(), "(add(.) + max(.))");
    }

    #[test]
    fn parse_split_join_pipeline() {
        let expr = parse_expr("split(\",\") | join(\"-\")").expect("must parse");
        assert_eq!(expr.to_string(), "split(\",\") | join(\"-\")");
    }

    #[test]
    fn parse_explicit_error_in_if() {
        let expr =
            parse_expr("if . == true or . == false then 1 else error end").expect("must parse");
        assert_eq!(
            expr.to_string(),
            "if ((. == true) or (. == false)) then 1 else error(.) end"
        );
    }
}
