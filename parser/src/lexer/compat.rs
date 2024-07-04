use std::io::Write;
use miette::{bail, IntoDiagnostic, Result};
use serde::{Deserialize, Serialize};

use crate::runpython::{default_python_path, spawn_python_script_command};

// Derived from:
// https://github.com/python/cpython/blob/main/Lib/token.py
#[allow(dead_code)]
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "UPPERCASE")]
pub enum PythonKind {
    EndMarker,
    Name,
    Number,
    String,
    NewLine,
    Indent,
    Dedent,
    LPar,
    RPar,
    LSqb,
    RSqb,
    Colon,
    Comma,
    Semi,
    Plus,
    Minus,
    Star,
    Slash,
    VBar,
    Amper,
    Less,
    Greater,
    Equal,
    Dot,
    Percent,
    LBrace,
    RBrace,
    EqEqual,
    NotEqual,
    LessEqual,
    GreaterEqual,
    Tilde,
    Circumflex,
    LeftShift,
    RightShift,
    DoubleStar,
    PlusEqual,
    MinEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,
    AmperEqual,
    VBarEqual,
    CircumflexEqual,
    LeftShiftEqual,
    RightShiftEqual,
    DoubleStarEqual,
    DoubleSlash,
    DoubleSlashEqual,
    At,
    AtEqual,
    RArrow,
    Ellipsis,
    ColonEqual,
    Exclamation,
    Op,
    Await,
    Async,
    #[serde(rename = "TYPE_IGNORE")]
    TypeIgnore,
    #[serde(rename = "TYPE_COMMENT")]
    TypeComment,
    #[serde(rename = "SOFT_KEYWORD")]
    SoftKeyword,
    #[serde(rename = "FSTRING_START")]
    FstringStart,
    #[serde(rename = "FSTRING_MIDDLE")]
    FstringMiddle,
    #[serde(rename = "FSTRING_END")]
    FstringEnd,
    Comment,
    NL,
    ErrorToken,
    Encoding,
    #[serde(rename = "N_TOKENS")]
    NTokens,
    #[serde(rename = "NT_OFFSET")]
    NTOffset,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct PythonToken {
    kind: PythonKind,
    value: String,
    start: (u32, u32),
    end: (u32, u32),
}

fn lex_python_source(source: &str) -> Result<Vec<PythonToken>> {
    let mut process = spawn_python_script_command(
        "parser/lex_python.py",
        vec!["--stdin", "--output-format", "json"],
        default_python_path()?,
    )?;

    // Get process stdin and write the input string.
    if let Some(mut stdin) = process.stdin.take() {
        stdin.write_all(source.as_bytes()).into_diagnostic()?;
    } else {
        bail!("Failed to open stdin when running `parser/lex_python.py`");
    }
    // Get process stdout and parse result.
    let output = process.wait_with_output().into_diagnostic()?;
    let python_tokens: Vec<PythonToken> = serde_json::from_str(String::from_utf8_lossy(&output.stdout).as_ref()).into_diagnostic()?;
    Ok(python_tokens)
}

#[cfg(test)]
mod tests {
    use tabled::{
        builder::Builder,
        settings::{Style, Width},
        settings::peaker::PriorityMax,
    };
    use terminal_size::{terminal_size, Width as TerminalWidth};
    use crate::{lexer::Lexer, token::Token};
    use crate::token::Kind;
    use super::{lex_python_source, PythonToken, PythonKind};

    #[test]
    fn test_simple_compat() {
        let source = r#"
a: int = 1
print(a)
"#;
        let mut lexer = Lexer::new(source);
        let enderpy_tokens = lexer.lex();
        let python_tokens = lex_python_source(source).unwrap();
        assert_tokens_eq(python_tokens, enderpy_tokens, &lexer);
    }

    fn python_tokenize_test_lexer(inputs: &[&str]) {
        for test_input in inputs.iter() {
            let mut lexer = Lexer::new(test_input);
            let tokens = lexer.lex();
            let python_tokens = lex_python_source(test_input).unwrap();
            assert_tokens_eq(python_tokens, tokens, &lexer);
        }
    }

    #[test]
    fn test_lex_operators() {
        python_tokenize_test_lexer(
            &[
                "1+2",
                "a+b",
                "a + b",
                "+=2",
                "xX = 2",
                "if else elif",
                "()",
                "[]",
                "{}:",
                ".",
                ",",
                ";",
                "@",
                "=",
                // TODO lex_python: Python lexer chokes on single backslash.
                // "\\",
                "#",
                "$",
                "?",
                "`",
                "->",
                "+=",
                "-=",
                "*=",
                "/=",
                "%=",
                "@=",
                "&=",
                "|=",
                "^=",
                "//=",
                "<<=",
                ">>=",
                "**=",
                "**",
                "//",
                "<<",
                ">>",
                "+",
                "-",
                "*",
                "**",
                "/",
                "//",
                "%",
                "@",
                "<<",
                ">>",
                "&",
                "|",
                "^",
                "~",
                ":=",
                "<",
                ">",
                "<=",
                ">=",
                "==",
                "!=",
            ],
        );
    }

    #[test]
    fn test_lex_keywords() {
        python_tokenize_test_lexer(
            &[
                "False None True and as assert async await",
                "break class continue def del elif else except",
                "finally for from global if import in is lambda",
                "nonlocal not or pass raise return try while with yield",
            ],
        );
    }

    #[test]
    fn test_lex_identifiers() {
        python_tokenize_test_lexer(
            &["a", "a_a", "_a", "a_", "a_a_a", "a_a_"],
        );
    }

    #[test]
    fn test_lex_literals() {
        // Binary
        python_tokenize_test_lexer(
            &[
                "0b0", "0b1", "0b10", "0b11", "0b100", "0b101", "0b110", "0b111",
            ],
        );

        // Octal
        python_tokenize_test_lexer(
            &["0o0", "0o1", "0o2", "0o3", "0o4", "0o5", "0o6", "0o7"],
        );

        // Hexadecimal
        python_tokenize_test_lexer(
            &[
                "0x0", "0x1", "0x2", "0x3", "0x4", "0x5", "0x6", "0x7", "0x8", "0x9", "0xa", "0xb",
                "0xc", "0xd", "0xe", "0xf", "0xA", "0xB", "0xC", "0xD", "0xE", "0xF",
            ],
        );

        // Point float
        python_tokenize_test_lexer(&["0.0 0.1 00.0 00.1 0.1j 0.01J"]);

        // Exponent float
        python_tokenize_test_lexer(&["0e0 0e-1 0e+2 0e+3j 0e+3J"]);

        // Integer
        python_tokenize_test_lexer(&["11 33 1j 1_000_000j"]);

        // Strings
        python_tokenize_test_lexer(
            &[
                "\"hello\"  ",
                "\"world\"",
                "\"\"",
                "a = \"hello\"",
                "'hello'",
                "\"\"\"hello\"\"\"",
                "'''hello'''",
            ],
        );

        // Bytes
        python_tokenize_test_lexer(
            &[
                "b\"hello\"",
                "b\"world\"",
                "b\"\"",
                "a = b\"hello\"",
                "b'hello'",
                "b\"\"\"hello\"\"\"",
                "b'''hello'''",
            ],
        );

        // Raw strings
        python_tokenize_test_lexer(
            &[
                "r\"hello\"",
                "r\"world\"",
                "r\"\"",
                "a = r\"hello\"",
                "r'hello'",
                "r\"\"\"hello\"\"\"",
                "r'''hello'''",
            ],
        );

        // Raw bytes
        python_tokenize_test_lexer(
            &[
                "rb\"hello\"",
                "rb\"world\"",
                "rb\"\"",
                "a = rb\"hello\"",
                "rb'hello'",
                "rb\"\"\"hello\"\"\"",
                "rb'''hello'''",
            ],
        );

        // Unicode strings
        python_tokenize_test_lexer(
            &[
                "u\"hello\"",
                "u\"world\"",
                "u\"\"",
                "a = u\"hello\"",
                "u'hello'",
                "u\"\"\"hello\"\"\"",
                "u'''hello'''",
            ],
        );
    }

    #[test]
    fn test_lex_imports() {
        python_tokenize_test_lexer(
            &["import a", "import a.b", "import a.b.c", "import a from b"],
        );
    }

    // TODO lex_python: Decide whether to keep this test or not. The Python lexer + enderpy lexer 
    // handle newlines in a nested context slightly differently.
    // - Python increments the row counter.
    // - enderpy appends them to the original row.
//     #[test]
//     fn test_lex_other() {
//         python_tokenize_test_lexer(
//             &["(a,
//
// )"],
//         );
//     }

    #[test]
    fn test_lex_indentation() {
        python_tokenize_test_lexer(
            &[
                "if True:
            pass\n",
                "if True:
    pass
else:
    pass",
                "if True:
    if True:
        pass
def",
                "def f(x):
    y = z

    print(y)
",
                "if a:

    f = c

    # Path: test_local.py
",
            ],
        );
    }

    #[test]
    fn test_lex_fstring() {
        python_tokenize_test_lexer(
            &[
                "f\"hello\"",
                "f'hello_{var}'",
                "f\"world\"",
                "f\"\"",
                "a = f\"hello\"",
                "f\"\"\"hello\"\"\"",
                "f'''hello'''",
                // TODO lex_python: Python lexes these poorly.
                // "f\"{{hey}}\"",
                // "f\"oh_{{hey}}\"",
                "f'a' 'c'",
                "f'hello_{f'''{a}'''}'",
            ],
        );

        // Raw F-strings
        python_tokenize_test_lexer(
            &[
                "rf\"hello\"",
                "rf\"world\"",
                "rf\"\"",
                "a = rf\"hello\"",
                "rf'hello_{var}'",
                "rf\"\"\"hello\"\"\"",
                "rf'''hello'''",
            ],
        );
    }

    #[test]
    fn test_lex_ellipsis() {
        python_tokenize_test_lexer(
            &[
                "...",
                "def a():
    ...",
            ],
        );
    }

    #[test]
    #[should_panic]
    fn test_lex_unterminated_string_double_quotes() {
        python_tokenize_test_lexer(
            &["\"hello", "'hello", "'''hello''", "'''hello'"],
        );
    }

    enum TokenMismatch {
        MissingToken(Option<PythonToken>, Option<Token>),
        WrongKind(PythonToken, Token),
        WrongValue(PythonToken, Token, String, String),
        WrongStartEnd(PythonToken, Token, (u32, u32), (u32, u32), (u32, u32), (u32, u32)),
    }

    fn assert_tokens_eq(python_tokens: Vec<PythonToken>, enderpy_tokens: Vec<Token>, lexer: &Lexer) {
        let num_python_tokens = python_tokens.len();
        let num_enderpy_tokens = enderpy_tokens.len();
        let last_index = std::cmp::max(num_python_tokens, num_enderpy_tokens) - 1;
        let mut mismatches: Vec<TokenMismatch> = vec![];
        for i in 0..last_index {
            let python_token = if i > num_python_tokens - 1 {
                None
            } else {
                Some(python_tokens[i].clone())
            };
            let enderpy_token = if i > num_enderpy_tokens - 1 {
                None
            } else {
                Some(enderpy_tokens[i].clone())
            };
            if python_token.is_none() || enderpy_token.is_none() {
                mismatches.push(TokenMismatch::MissingToken(python_token, enderpy_token));
                continue;
            }
            let python_token = python_token.unwrap();
            let enderpy_token = enderpy_token.unwrap();
            if let Some(mismatch) = check_tokens_match(python_token, enderpy_token, lexer) {
                if check_mismatch_from_python_trailing_newline(&mismatch, &python_tokens[i + 1..]) {
                    // If we found Python's trailing newline, we've read the end of file.
                    break;
                }
                mismatches.push(mismatch);
            }
        }
        if mismatches.is_empty() {
            return;
        }

        let include_source = std::env::var("INCLUDE_SOURCE").is_ok();
        let include_all_tokens = std::env::var("INCLUDE_ALL_TOKENS").is_ok();

        let mut table_builder = Builder::default();
        // Add the table header.
        let mut header = vec!["Python token", "Enderpy token", "Failure"];
        if include_source {
            header.push("Source");
        }
        table_builder.push_record(header);
        // Add the table rows. Each row represents a lexer token mismatch.
        let num_mismatches = mismatches.len();
        for mismatch in mismatches {
            let mut row: Vec<String> = vec![];
            let (python_token, enderpy_token, message) = match mismatch {
                TokenMismatch::MissingToken(python_token, enderpy_token) => {
                    (python_token, enderpy_token, "Missing token".to_string())
                },
                TokenMismatch::WrongKind(python_token, enderpy_token) => {
                    let message = format!("Wrong token kind.\nExpected: {:?}\nActual: {:?}", python_token.kind, enderpy_token.kind);
                    (
                        Some(python_token),
                        Some(enderpy_token),
                        message,
                    )
                },
                TokenMismatch::WrongValue(python_token, enderpy_token, expected_value, actual_value) => {
                    (Some(python_token), Some(enderpy_token), format!("Wrong token value.\nExpected: {:?}\nActual: {:?}", expected_value, actual_value))
                },
                TokenMismatch::WrongStartEnd(python_token, enderpy_token, expected_start, expected_end, actual_start, actual_end) => {
                    (
                        Some(python_token),
                        Some(enderpy_token),
                        format!(
                            "Wrong token start/end offset.\nExpected: {:?} - {:?}\nActual: {:?} - {:?}",
                            expected_start,
                            expected_end,
                            actual_start,
                            actual_end,
                        ),
                    )
                },
            };
            if include_all_tokens {
                row.extend_from_slice(&[
                    python_tokens
                        .iter()
                        .map(|token| {
                            let is_this_token =  python_token.as_ref().is_some_and(|tok| tok == token);
                            format!("{}{:?}", if is_this_token { "→ " } else { "" }, token)
                        })
                        .collect::<Vec<String>>()
                        .join("\n"),
                    enderpy_tokens
                        .iter()
                        .map(|token| {
                            let is_this_token = enderpy_token.as_ref().is_some_and(|tok| tok == token);
                            format!("{}{:?}", if is_this_token { "→ " } else { "" }, token)
                        })
                        .collect::<Vec<String>>()
                        .join("\n"),
                    message,
                ]);
            } else {
                row.extend_from_slice(&[
                    python_token.map_or("None".to_string(), |t| format!("{:?}", t)),
                    enderpy_token.map_or("None".to_string(), |t| format!("{:?}", t)),
                    message,
                ]);
            }
            if include_source {
                row.push(lexer.source.to_string());
            }
            table_builder.push_record(row);
        }
        let mut table = table_builder.build();
        let (TerminalWidth(width), _) = terminal_size().expect("Unable to determine terminal size.");
        table
            .with(Style::modern())
            .with(Width::wrap(width as usize).keep_words().priority::<PriorityMax>())
            .with(Width::increase(width as usize));
        panic!("enderpy tokens do not match Python tokens.\n{}\n{} token mismatches found", table, num_mismatches);
    }

    fn check_tokens_match(python_token: PythonToken, enderpy_token: Token, lexer: &Lexer) -> Option<TokenMismatch> {
        let kind_matches = match python_token.kind {
            PythonKind::EndMarker => enderpy_token.kind == Kind::Eof,
            // For some reason, Python maintains a kind for these tokens but doesn't use them
            // during tokenization.
            // Instead, it slams keywords together into a generic Name kind.
            PythonKind::Name => {
                matches_python_name_token(python_token.value.as_str(), &enderpy_token.kind)
            },
            PythonKind::Number => matches!(enderpy_token.kind, Kind::Integer | Kind::PointFloat | Kind::ExponentFloat | Kind::Binary | Kind::Octal | Kind::Hexadecimal | Kind::ImaginaryPointFloat | Kind::ImaginaryExponentFloat | Kind::ImaginaryInteger),
            // NOTE: The Python tokenizer doesn't appear to track differences in string modifiers.
            // For example, "hello"/u"hello"/r"hello" are all just String.
            PythonKind::String => matches!(enderpy_token.kind, Kind::StringLiteral | Kind::Bytes | Kind::RawBytes | Kind::Unicode),
            PythonKind::NewLine => enderpy_token.kind == Kind::NewLine,
            PythonKind::Indent => enderpy_token.kind == Kind::Indent,
            PythonKind::Dedent => enderpy_token.kind == Kind::Dedent,
            PythonKind::LPar => enderpy_token.kind == Kind::LeftParen,
            PythonKind::RPar => enderpy_token.kind == Kind::RightParen,
            PythonKind::LSqb => enderpy_token.kind == Kind::LeftBrace,
            PythonKind::RSqb => enderpy_token.kind == Kind::RightBrace,
            PythonKind::Colon => enderpy_token.kind == Kind::Colon,
            PythonKind::Comma => enderpy_token.kind == Kind::Comma,
            PythonKind::Semi => enderpy_token.kind == Kind::SemiColon,
            PythonKind::Plus => enderpy_token.kind == Kind::Plus,
            PythonKind::Minus => enderpy_token.kind == Kind::Minus,
            PythonKind::Star => enderpy_token.kind == Kind::Mul,
            PythonKind::Slash => enderpy_token.kind == Kind::Div,
            PythonKind::VBar => enderpy_token.kind == Kind::BitOr,
            PythonKind::Amper => enderpy_token.kind == Kind::BitAnd,
            PythonKind::Less => enderpy_token.kind == Kind::Less,
            PythonKind::Greater => enderpy_token.kind == Kind::Greater,
            PythonKind::Equal => enderpy_token.kind == Kind::Assign,
            PythonKind::Dot => enderpy_token.kind == Kind::Dot,
            PythonKind::Percent => enderpy_token.kind == Kind::Mod,
            PythonKind::LBrace => enderpy_token.kind == Kind::LeftBracket,
            PythonKind::RBrace => enderpy_token.kind == Kind::RightBracket,
            PythonKind::EqEqual => enderpy_token.kind == Kind::Eq,
            PythonKind::NotEqual => enderpy_token.kind == Kind::NotEq,
            PythonKind::LessEqual => enderpy_token.kind == Kind::LessEq,
            PythonKind::GreaterEqual => enderpy_token.kind == Kind::GreaterEq,
            PythonKind::Tilde => enderpy_token.kind == Kind::BitNot,
            PythonKind::Circumflex => enderpy_token.kind == Kind::BitXor,
            PythonKind::LeftShift => enderpy_token.kind == Kind::LeftShift,
            PythonKind::RightShift => enderpy_token.kind == Kind::RightShift,
            PythonKind::DoubleStar => enderpy_token.kind == Kind::Pow,
            PythonKind::PlusEqual => enderpy_token.kind == Kind::AddAssign,
            PythonKind::MinEqual => enderpy_token.kind == Kind::SubAssign,
            PythonKind::StarEqual => enderpy_token.kind == Kind::MulAssign,
            PythonKind::SlashEqual => enderpy_token.kind == Kind::DivAssign,
            PythonKind::PercentEqual => enderpy_token.kind == Kind::ModAssign,
            PythonKind::AmperEqual => enderpy_token.kind == Kind::BitAndAssign,
            PythonKind::VBarEqual => enderpy_token.kind == Kind::BitOrAssign,
            PythonKind::CircumflexEqual => enderpy_token.kind == Kind::BitXorAssign,
            PythonKind::LeftShiftEqual => enderpy_token.kind == Kind::ShiftLeftAssign,
            PythonKind::RightShiftEqual => enderpy_token.kind == Kind::ShiftRightAssign,
            PythonKind::DoubleStarEqual => enderpy_token.kind == Kind::PowAssign,
            PythonKind::DoubleSlash => enderpy_token.kind == Kind::IntDiv,
            PythonKind::DoubleSlashEqual => enderpy_token.kind == Kind::IntDivAssign,
            PythonKind::At => enderpy_token.kind == Kind::MatrixMul,
            PythonKind::AtEqual => enderpy_token.kind == Kind::MatrixMulAssign,
            PythonKind::RArrow => enderpy_token.kind == Kind::Arrow,
            PythonKind::Ellipsis => enderpy_token.kind == Kind::Ellipsis,
            PythonKind::ColonEqual => enderpy_token.kind == Kind:: Walrus,
            PythonKind::Exclamation => false, // doesn't exist
            // For some reason, Python maintains a kind for these tokens but doesn't use them
            // during tokenization.
            // Instead, it slams all operators together into a generic Op kind.
            PythonKind::Op => {
                matches_python_op_token(python_token.value.as_str(), &enderpy_token.kind)
            },
            PythonKind::Await => enderpy_token.kind == Kind::Await,
            PythonKind::Async => enderpy_token.kind == Kind::Async,
            PythonKind::TypeIgnore => false, // doesn't exist
            PythonKind::TypeComment => false, // doesn't exist
            PythonKind::SoftKeyword => false, // doesn't exist
            PythonKind::FstringStart => matches!(enderpy_token.kind, Kind::FStringStart | Kind::RawFStringStart),
            PythonKind::FstringMiddle => enderpy_token.kind == Kind::FStringMiddle,
            PythonKind::FstringEnd => enderpy_token.kind == Kind::FStringEnd,
            PythonKind::Comment => enderpy_token.kind == Kind::Comment,
            // In Python, this represents a line break within a single statement. We don't
            // currently make this distinction.
            PythonKind::NL => enderpy_token.kind == Kind::NewLine,
            PythonKind::ErrorToken => enderpy_token.kind == Kind::Error,
            PythonKind::Encoding => false, // doesn't exist
            PythonKind::NTokens => false, // doesn't exist,
            PythonKind::NTOffset => false, // doesn't exist
        };
        if !kind_matches {
            return Some(TokenMismatch::WrongKind(python_token, enderpy_token));
        }

        let python_token_value = python_token.value.clone();
        let enderpy_token_value = enderpy_token.value.to_string();
        // The Python tokenizer sets values in a number of places where enderpy simply relies
        // on kind to assume value. Handle those cases here.
        let value_matches = matches_python_name_token(python_token.value.as_str(), &enderpy_token.kind)
            || matches_python_op_token(python_token.value.as_str(), &enderpy_token.kind)
            || matches_python_indent_dedent_token(&python_token.kind, &enderpy_token.kind)
            || (python_token.kind == PythonKind::EndMarker && enderpy_token.kind == Kind::Eof)
            || (python_token.value.as_str() == "\n" && enderpy_token.kind == Kind::NewLine)
            || python_token_value == enderpy_token_value;
        if !value_matches {
            return Some(TokenMismatch::WrongValue(python_token, enderpy_token, python_token_value, enderpy_token_value));
        }

        let (mut enderpy_start_row, enderpy_start_col) = lexer.to_row_col(enderpy_token.start);
        let (mut enderpy_end_row, mut enderpy_end_col) = lexer.to_row_col(enderpy_token.end);
        // Python reserves the first row for a file encoding when detokenizing, so we add one
        // to our row values to match.
        enderpy_start_row += 1;
        enderpy_end_row += 1;
        if enderpy_token.kind == Kind::NewLine {
            // enderpy has newline tokens span from the end of the first line to the beginning of
            // the next line.
            // Python adds the token to the end of the first line.
            enderpy_end_row = enderpy_start_row;
            enderpy_end_col = enderpy_start_col + 1;
        }
        let python_token_start = python_token.start;
        let python_token_end = python_token.end;
        if enderpy_start_row != python_token_start.0 || enderpy_start_col != python_token_start.1 || enderpy_end_row != python_token_end.0 || enderpy_end_col != python_token_end.1 {
            return Some(TokenMismatch::WrongStartEnd(
                python_token,
                enderpy_token,
                python_token_start,
                python_token_end,
                (enderpy_start_row, enderpy_start_col),
                (enderpy_end_row, enderpy_end_col),
            ));
        }
        None
    }

    fn matches_python_name_token(python_token_value: &str, token_kind: &Kind) -> bool {
        match python_token_value {
            "if" => token_kind == &Kind::If,
            "elif" => token_kind == &Kind::Elif,
            "else" => token_kind == &Kind::Else,
            "False" => token_kind == &Kind::False,
            "None" => token_kind == &Kind::None,
            "True" => token_kind == &Kind::True,
            "and" => token_kind == &Kind::And,
            "as" => token_kind == &Kind::As,
            "assert" => token_kind == &Kind::Assert,
            "async" => token_kind == &Kind::Async,
            "await" => token_kind == &Kind::Await,
            "break" => token_kind == &Kind::Break,
            "class" => token_kind == &Kind::Class,
            "continue" => token_kind == &Kind::Continue,
            "def" => token_kind == &Kind::Def,
            "del" => token_kind == &Kind::Del,
            "except" => token_kind == &Kind::Except,
            "finally" => token_kind == &Kind::Finally,
            "for" => token_kind == &Kind::For,
            "from" => token_kind == &Kind::From,
            "global" => token_kind == &Kind::Global,
            "import" => token_kind == &Kind::Import,
            "in" => token_kind == &Kind::In,
            "is" => token_kind == &Kind::Is,
            "lambda" => token_kind == &Kind::Lambda,
            "nonlocal" => token_kind == &Kind::Nonlocal,
            "not" => token_kind == &Kind::Not,
            "or" => token_kind == &Kind::Or,
            "pass" => token_kind == &Kind::Pass,
            "raise" => token_kind == &Kind::Raise,
            "return" => token_kind == &Kind::Return,
            "try" => token_kind == &Kind::Try,
            "while" => token_kind == &Kind::While,
            "with" => token_kind == &Kind::With,
            "yield" => token_kind == &Kind::Yield,
            _ => token_kind == &Kind::Identifier,
        }
    }

    fn matches_python_op_token(python_token_value: &str, token_kind: &Kind) -> bool {
        match python_token_value {
            "!=" => token_kind == &Kind::NotEq,
            "$" => token_kind == &Kind::Dollar,
            "%" => token_kind == &Kind::Mod,
            "%=" => token_kind == &Kind::ModAssign,
            "&" => token_kind == &Kind::BitAnd,
            "&=" => token_kind == &Kind::BitAndAssign,
            "(" => token_kind == &Kind::LeftParen,
            ")" => token_kind == &Kind::RightParen,
            "*" => token_kind == &Kind::Mul,
            "**" => token_kind == &Kind::Pow,
            "**=" => token_kind == &Kind::PowAssign,
            "*=" => token_kind == &Kind::MulAssign,
            "+" => token_kind == &Kind::Plus,
            "+=" => token_kind == &Kind::AddAssign,
            "," => token_kind == &Kind::Comma,
            "-" => token_kind == &Kind::Minus,
            "-=" => token_kind == &Kind::SubAssign,
            "->" => token_kind == &Kind::Arrow,
            "." => token_kind == &Kind::Dot,
            "/" => token_kind == &Kind::Div,
            "//" => token_kind == &Kind::IntDiv,
            "//=" => token_kind == &Kind::IntDivAssign,
            "/=" => token_kind == &Kind::DivAssign,
            ":" => token_kind == &Kind::Colon,
            ":=" => token_kind == &Kind::Walrus,
            ";" => token_kind == &Kind::SemiColon,
            "<" => token_kind == &Kind::Less,
            "<<" => token_kind == &Kind::LeftShift,
            "<<=" => token_kind == &Kind::ShiftLeftAssign,
            "<=" => token_kind == &Kind::LessEq,
            "=" => token_kind == &Kind::Assign,
            "==" => token_kind == &Kind::Eq,
            ">" => token_kind == &Kind::Greater,
            ">=" => token_kind == &Kind::GreaterEq,
            ">>" => token_kind == &Kind::RightShift,
            ">>=" => token_kind == &Kind::ShiftRightAssign,
            "?" => token_kind == &Kind::QuestionMark,
            "@" => token_kind == &Kind::MatrixMul,
            "@=" => token_kind == &Kind::MatrixMulAssign,
            "[" => token_kind == &Kind::LeftBrace,
            "]" => token_kind == &Kind::RightBrace,
            "^" => token_kind == &Kind::BitXor,
            "^=" => token_kind == &Kind::BitXorAssign,
            "`" => token_kind == &Kind::BackTick,
            "{" => token_kind == &Kind::LeftBracket,
            "|" => token_kind == &Kind::BitOr,
            "|=" => token_kind == &Kind::BitOrAssign,
            "}" => token_kind == &Kind::RightBracket,
            "~" => token_kind == &Kind::BitNot,
            "..." => token_kind == &Kind::Ellipsis,
            _ => false,
        }
    }

    fn matches_python_indent_dedent_token(python_kind: &PythonKind, enderpy_kind: &Kind) -> bool {
        // TODO lex_python: There's no obvious way with the Python lexer to determine what is
        // considered one indent level. Instead, it simply stores the literal whitespace. This
        // makes it really difficult to determine whether indentation levels actually match
        // (without looking around at the larger context), so for now we'll just make sure the
        // Kind lines up.
        (python_kind == &PythonKind::Indent && enderpy_kind == &Kind::Indent) || (python_kind == &PythonKind::Dedent && enderpy_kind == &Kind::Dedent)
    }

    /// The Python tokenizer adds a cheeky newline to the end of the source, causing mismatches. We
    /// handle this by ignoring mismatches that meet all of the following criteria.
    /// - The mismatch type is `WrongKind`.
    /// - The Python kind is a known whitespace value.
    /// - The enderpy kind is a EOF.
    /// - The only remaining Python tokens before EOF are known whitespace values.
    fn check_mismatch_from_python_trailing_newline(mismatch: &TokenMismatch, remaining_tokens: &[PythonToken]) -> bool {
        if let TokenMismatch::WrongKind(python_token, enderpy_token) = mismatch {
            if !matches!(python_token.kind, PythonKind::NewLine | PythonKind::NL) || enderpy_token.kind != Kind::Eof {
                return false;
            }
            return remaining_tokens.iter().all(|t| matches!(t.kind, PythonKind::NewLine | PythonKind::NL | PythonKind::Dedent | PythonKind::EndMarker));
        }
        false
    }
}
