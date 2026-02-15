use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Definition {
    pub name: String,
    pub params: Option<String>,
    pub body: String,
    pub line: usize,
}

impl Definition {
    pub fn cache_key(&self) -> String {
        match &self.params {
            Some(params) => format!("{}({})", self.name, normalize_ws(params)),
            None => self.name.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DefParseError {
    pub line: usize,
    pub message: String,
}

impl fmt::Display for DefParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {}: {}", self.line, self.message)
    }
}

pub fn parse_definitions(source: &str) -> Result<Vec<Definition>, DefParseError> {
    let bytes = source.as_bytes();
    let mut out = Vec::new();
    let mut i = 0usize;

    while i < bytes.len() {
        skip_ws_and_comments(bytes, &mut i);
        if i >= bytes.len() {
            break;
        }

        if !starts_with_keyword(bytes, i, "def") {
            return Err(DefParseError {
                line: line_of(source, i),
                message: "expected `def`".to_string(),
            });
        }
        i += 3;

        skip_ws(bytes, &mut i);
        let name = parse_identifier(source, bytes, &mut i)?;
        skip_ws(bytes, &mut i);

        let params = if i < bytes.len() && bytes[i] == b'(' {
            Some(parse_param_list(source, bytes, &mut i)?)
        } else {
            None
        };

        skip_ws(bytes, &mut i);
        if i >= bytes.len() || bytes[i] != b':' {
            return Err(DefParseError {
                line: line_of(source, i.min(bytes.len().saturating_sub(1))),
                message: "expected `:` after definition name".to_string(),
            });
        }
        i += 1;

        let body_start = i;
        let mut nested_defs = 0usize;
        let mut depth_paren = 0usize;
        let mut depth_bracket = 0usize;
        let mut depth_brace = 0usize;
        let mut in_string = false;
        let mut escaped = false;
        let mut body_end = None;

        while i < bytes.len() {
            let b = bytes[i];

            if in_string {
                if escaped {
                    escaped = false;
                    i += 1;
                    continue;
                }
                if b == b'\\' {
                    escaped = true;
                } else if b == b'"' {
                    in_string = false;
                }
                i += 1;
                continue;
            }

            match b {
                b'"' => {
                    in_string = true;
                    i += 1;
                }
                b'#' => {
                    i += 1;
                    while i < bytes.len() && bytes[i] != b'\n' {
                        i += 1;
                    }
                }
                b'(' => {
                    depth_paren += 1;
                    i += 1;
                }
                b')' => {
                    if depth_paren > 0 {
                        depth_paren -= 1;
                    }
                    i += 1;
                }
                b'[' => {
                    depth_bracket += 1;
                    i += 1;
                }
                b']' => {
                    if depth_bracket > 0 {
                        depth_bracket -= 1;
                    }
                    i += 1;
                }
                b'{' => {
                    depth_brace += 1;
                    i += 1;
                }
                b'}' => {
                    if depth_brace > 0 {
                        depth_brace -= 1;
                    }
                    i += 1;
                }
                _ => {
                    let top_level = depth_paren == 0 && depth_bracket == 0 && depth_brace == 0;
                    if top_level && starts_with_keyword(bytes, i, "def") {
                        nested_defs += 1;
                        i += 3;
                        continue;
                    }

                    if top_level && b == b';' {
                        if nested_defs > 0 {
                            nested_defs -= 1;
                            i += 1;
                        } else {
                            body_end = Some(i);
                            i += 1;
                            break;
                        }
                    } else {
                        i += 1;
                    }
                }
            }
        }

        let Some(body_end) = body_end else {
            return Err(DefParseError {
                line: line_of(source, body_start),
                message: format!("unterminated definition `{name}`"),
            });
        };

        let body = source[body_start..body_end].trim().to_string();
        out.push(Definition {
            name,
            params,
            body,
            line: line_of(source, body_start),
        });
    }

    Ok(out)
}

fn parse_identifier(source: &str, bytes: &[u8], i: &mut usize) -> Result<String, DefParseError> {
    if *i >= bytes.len() || !is_ident_start(bytes[*i]) {
        return Err(DefParseError {
            line: line_of(source, (*i).min(bytes.len().saturating_sub(1))),
            message: "expected identifier".to_string(),
        });
    }
    let start = *i;
    *i += 1;
    while *i < bytes.len() && is_ident_continue(bytes[*i]) {
        *i += 1;
    }
    Ok(source[start..*i].to_string())
}

fn parse_param_list(source: &str, bytes: &[u8], i: &mut usize) -> Result<String, DefParseError> {
    debug_assert!(*i < bytes.len() && bytes[*i] == b'(');
    let start = *i + 1;
    *i += 1;
    let mut depth = 1usize;
    let mut in_string = false;
    let mut escaped = false;

    while *i < bytes.len() {
        let b = bytes[*i];
        if in_string {
            if escaped {
                escaped = false;
                *i += 1;
                continue;
            }
            if b == b'\\' {
                escaped = true;
            } else if b == b'"' {
                in_string = false;
            }
            *i += 1;
            continue;
        }

        match b {
            b'"' => {
                in_string = true;
                *i += 1;
            }
            b'(' => {
                depth += 1;
                *i += 1;
            }
            b')' => {
                depth -= 1;
                if depth == 0 {
                    let out = source[start..*i].trim().to_string();
                    *i += 1;
                    return Ok(out);
                }
                *i += 1;
            }
            _ => {
                *i += 1;
            }
        }
    }

    Err(DefParseError {
        line: line_of(source, start),
        message: "unterminated parameter list".to_string(),
    })
}

fn starts_with_keyword(bytes: &[u8], i: usize, kw: &str) -> bool {
    let kw_bytes = kw.as_bytes();
    if i + kw_bytes.len() > bytes.len() {
        return false;
    }
    if &bytes[i..i + kw_bytes.len()] != kw_bytes {
        return false;
    }

    let prev_ok = if i == 0 {
        true
    } else {
        !is_ident_continue(bytes[i - 1])
    };
    let next_idx = i + kw_bytes.len();
    let next_ok = if next_idx >= bytes.len() {
        true
    } else {
        !is_ident_continue(bytes[next_idx])
    };
    prev_ok && next_ok
}

fn normalize_ws(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn skip_ws(bytes: &[u8], i: &mut usize) {
    while *i < bytes.len() && bytes[*i].is_ascii_whitespace() {
        *i += 1;
    }
}

fn skip_ws_and_comments(bytes: &[u8], i: &mut usize) {
    loop {
        skip_ws(bytes, i);
        if *i < bytes.len() && bytes[*i] == b'#' {
            *i += 1;
            while *i < bytes.len() && bytes[*i] != b'\n' {
                *i += 1;
            }
            continue;
        }
        break;
    }
}

fn is_ident_start(b: u8) -> bool {
    b.is_ascii_alphabetic() || b == b'_'
}

fn is_ident_continue(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

fn line_of(source: &str, offset: usize) -> usize {
    source[..offset.min(source.len())]
        .bytes()
        .filter(|b| *b == b'\n')
        .count()
        + 1
}

#[cfg(test)]
mod tests {
    use super::parse_definitions;

    #[test]
    fn parses_simple_defs() {
        let src = r#"
            # comment
            def true: 0 == 0;
            def false: 0 != 0;
        "#;
        let defs = parse_definitions(src).expect("must parse");
        assert_eq!(defs.len(), 2);
        assert_eq!(defs[0].cache_key(), "true");
        assert_eq!(defs[0].body, "0 == 0");
        assert_eq!(defs[1].cache_key(), "false");
        assert_eq!(defs[1].body, "0 != 0");
    }

    #[test]
    fn handles_nested_defs_in_body() {
        let src = r#"
            def repeat(f): def rec: f, rec; rec;
        "#;
        let defs = parse_definitions(src).expect("must parse");
        assert_eq!(defs.len(), 1);
        assert_eq!(defs[0].cache_key(), "repeat(f)");
        assert_eq!(defs[0].body, "def rec: f, rec; rec");
    }

    #[test]
    fn normalizes_spacing_in_param_cache_key() {
        let src = r#"
            def split (re; flags): split_(re; flags + "g");
        "#;
        let defs = parse_definitions(src).expect("must parse");
        assert_eq!(defs.len(), 1);
        assert_eq!(defs[0].cache_key(), "split(re; flags)");
    }
}
