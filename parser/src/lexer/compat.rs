use std::io::Write;
use miette::{bail, IntoDiagnostic, Result};
use serde::{Deserialize, Serialize};

use crate::runpython::{default_python_path, spawn_python_script_command};

#[allow(dead_code)]
#[derive(Debug, Serialize, Deserialize, Clone)]
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
    // // Special type for conversion.
    // Unknown,
}

// impl From<Kind> for PythonKind {
//     fn from(kind: Kind) -> Self {
//         match kind {
//             Kind::NewLine => PythonKind::NewLine,
//             Kind::Indent => PythonKind::Indent,
//             Kind::Dedent => PythonKind::Dedent,
//             Kind::Identifier => PythonKind::Unknown,
//             Kind::False => PythonKind::Unknown,
//             Kind::None => PythonKind::Unknown,
//             Kind::True => PythonKind::Unknown,
//             Kind::And => PythonKind::Unknown,
//             Kind::As => PythonKind::Unknown,
//             Kind::Assert => PythonKind::Unknown,
//             Kind::Async => PythonKind::Async,
//             Kind::Await => PythonKind::Await,
//             Kind::Break => PythonKind::Unknown,
//             Kind::Class => PythonKind::Unknown,
//             Kind::Continue => PythonKind::Unknown,
//             Kind::Comment => PythonKind::Comment,
//             Kind::Def => PythonKind::Unknown,
//             Kind::Del => PythonKind::Unknown,
//             Kind::Elif => PythonKind::Unknown,
//             Kind::Else => PythonKind::Unknown,
//             Kind::Ellipsis => PythonKind::Ellipsis,
//             Kind::Except => PythonKind::Unknown,
//             Kind::Finally => PythonKind::Unknown,
//             Kind::For => PythonKind::Unknown,
//             Kind::From => PythonKind::Unknown,
//             Kind::Global => PythonKind::Unknown,
//             Kind::If => PythonKind::Unknown,
//             Kind::Import => PythonKind::Unknown,
//             Kind::In => PythonKind::Unknown,
//             Kind::Is => PythonKind::Unknown,
//             Kind::Lambda => PythonKind::Unknown,
//             Kind::Nonlocal => PythonKind::Unknown,
//             Kind::Not => PythonKind::Unknown,
//             Kind::Or => PythonKind::Unknown,
//             Kind::Pass => PythonKind::Unknown,
//             Kind::Raise => PythonKind::Unknown,
//             Kind::Return => PythonKind::Unknown,
//             Kind::Try => PythonKind::Unknown,
//             Kind::While => PythonKind::Unknown,
//             Kind::With => PythonKind::Unknown,
//             Kind::Yield => PythonKind::Unknown,
//             Kind::StringLiteral => PythonKind::Unknown,
//             Kind::FStringStart => PythonKind::FstringStart,
//             Kind::FStringMiddle => PythonKind::FstringMiddle,
//             Kind::FStringEnd => PythonKind::FstringEnd,
//             Kind::Bytes => PythonKind::Unknown,
//             Kind::RawFStringStart => PythonKind::Unknown,
//             Kind::RawBytes => PythonKind::Unknown,
//             Kind::Unicode => PythonKind::Unknown,
//             Kind::Integer => PythonKind::Number,
//             Kind::Binary => PythonKind::Unknown,
//             Kind::Octal => PythonKind::Unknown,
//             Kind::Hexadecimal => PythonKind::Unknown,
//             Kind::PointFloat => PythonKind::Number,
//             Kind::ExponentFloat => PythonKind::Number,
//             Kind::ImaginaryInteger => PythonKind::Unknown,
//             Kind::ImaginaryPointFloat => PythonKind::Unknown,
//             Kind::ImaginaryExponentFloat => PythonKind::Unknown,
//             Kind::Plus => PythonKind::Plus,
//             Kind::Minus => PythonKind::Minus,
//             Kind::Mul => PythonKind::Star,
//             Kind::Pow => PythonKind::DoubleStar,
//             Kind::Div => PythonKind::Slash,
//             Kind::IntDiv => PythonKind::DoubleSlash,
//             Kind::Mod => PythonKind::Percent,
//             Kind::MatrixMul => PythonKind::At,
//             Kind::LeftShift => PythonKind::LeftShift,
//             Kind::RightShift => PythonKind::RightShift,
//             Kind::BitAnd => PythonKind::Amper,
//             Kind::BitOr => PythonKind::VBar,
//             Kind::BitXor => PythonKind::Circumflex,
//             Kind::BitNot => PythonKind::Tilde,
//             Kind::Walrus => PythonKind::ColonEqual,
//             Kind::Less => PythonKind::Less,
//             Kind::Greater => PythonKind::Greater,
//             Kind::LessEq => PythonKind::LessEqual,
//             Kind::GreaterEq => PythonKind::GreaterEqual,
//             Kind::Eq => PythonKind::Equal,
//             Kind::NotEq => PythonKind::NotEqual,
//             Kind::LeftParen => PythonKind::LPar,
//             Kind::RightParen => PythonKind::RPar,
//             Kind::LeftBrace => PythonKind::LSqb,
//             Kind::RightBrace => PythonKind::RSqb,
//             Kind::LeftBracket => PythonKind::LBrace,
//             Kind::RightBracket => PythonKind::RBrace,
//             Kind::Comma => PythonKind::Comma,
//             Kind::Colon => PythonKind::Colon,
//             Kind::Dot => PythonKind::Dot,
//             Kind::SemiColon => PythonKind::Semi,
//             Kind::Assign => PythonKind::Equal,
//             Kind::Arrow => PythonKind::RArrow,
//             Kind::AddAssign => PythonKind::PlusEqual,
//             Kind::SubAssign => PythonKind::MinEqual,
//             Kind::MulAssign => PythonKind::StarEqual,
//             Kind::DivAssign => PythonKind::SlashEqual,
//             Kind::ModAssign => PythonKind::PercentEqual,
//             Kind::MatrixMulAssign => PythonKind::AtEqual,
//             Kind::BitAndAssign => PythonKind::AmperEqual,
//             Kind::BitOrAssign => PythonKind::VBarEqual,
//             Kind::BitXorAssign => PythonKind::CircumflexEqual,
//             Kind::IntDivAssign => PythonKind::DoubleSlashEqual,
//             Kind::ShiftLeftAssign => PythonKind::LeftShiftEqual,
//             Kind::ShiftRightAssign => PythonKind::RightShiftEqual,
//             Kind::PowAssign => PythonKind::DoubleStarEqual,
//             Kind::BackSlash => PythonKind::Unknown,
//             Kind::Dollar => PythonKind::Unknown,
//             Kind::QuestionMark => PythonKind::Unknown,
//             Kind::BackTick => PythonKind::Unknown,
//             Kind::WhiteSpace => PythonKind::Unknown,
//             Kind::Error => PythonKind::ErrorToken,
//             Kind::Eof => PythonKind::EndMarker,
//         }
//     }
// }

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct PythonToken {
    kind: PythonKind,
    value: String,
    start: (u32, u32),
    end: (u32, u32),
}

pub fn lex_python_source(source: &str) -> Result<Vec<PythonToken>> {
    let mut process = spawn_python_script_command(
        "parser/lex_python.py",
        vec!["--stdin", "--output-format", "json"],
        default_python_path()?,
    )?;

    // Get stdin handle to Python process
    if let Some(mut stdin) = process.stdin.take() {
        // Write the input string to Python script's stdin
        stdin.write_all(source.as_bytes()).into_diagnostic()?;
    } else {
        bail!("Failed to open stdin when running `parser/lex_python.py`");
    }
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
        let mut enderpy_tokens = vec![];
        loop {
            let token = lexer.next_token();
            let token_kind = token.kind;
            enderpy_tokens.push(token);
            if token_kind == Kind::Eof {
                break;
            }
        }
        let python_tokens = lex_python_source(source).unwrap();
        assert_tokens_eq(python_tokens, enderpy_tokens, &lexer);
    }

    enum TokenMismatch {
        MissingToken(Option<PythonToken>, Option<Token>),
        WrongKind(PythonToken, Token),
        WrongValue(PythonToken, Token, String, String),
        WrongStartEnd(PythonToken, Token, (u32, u32), (u32, u32), (u32, u32), (u32, u32)),
    }

    impl TokenMismatch {
        fn to_table_row(&self) -> [String; 3] {
            match self {
                TokenMismatch::MissingToken(python_token, enderpy_token) => {
                    [
                        format!("{:?}", python_token),
                        format!("{:?}", enderpy_token),
                        "Missing token".to_string(),
                    ]
                },
                TokenMismatch::WrongKind(python_token, enderpy_token) => {
                    [
                        format!("{:?}", python_token),
                        format!("{:?}", enderpy_token),
                        format!("Wrong token kind.\nExpected: {:?}\nActual: {:?}", python_token.kind, enderpy_token.kind),
                    ]
                },
                TokenMismatch::WrongValue(python_token, enderpy_token, expected_value, actual_value) => {
                    [
                        format!("{:?}", python_token),
                        format!("{:?}", enderpy_token),
                        format!("Wrong token value.\nExpected: {:?}\nActual: {:?}", expected_value, actual_value),
                    ]
                },
                TokenMismatch::WrongStartEnd(python_token, enderpy_token, expected_start, expected_end, actual_start, actual_end) => {
                    [
                        format!("{:?}", python_token),
                        format!("{:?}", enderpy_token),
                        format!(
                            "Wrong token start/end offset.\nExpected: {:?} - {:?}\nActual: {:?} - {:?}",
                            expected_start,
                            expected_end,
                            actual_start,
                            actual_end,
                        ),
                    ]
                },
            }
        }
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
                mismatches.push(mismatch);
            }
        }
        if mismatches.is_empty() {
            return;
        }
        let mut table_builder = Builder::default();
        table_builder.push_record(["Python token", "enderpy token", "Failure"]);
        let num_mismatches = mismatches.len();
        for mismatch in mismatches {
            table_builder.push_record(mismatch.to_table_row());
        }
        let mut table = table_builder.build();
        let (TerminalWidth(width), _) = terminal_size().expect("Unable to determine terminal size.");
        table
            .with(Style::modern())
            .with(Width::wrap(width as usize).priority::<PriorityMax>())
            .with(Width::increase(width as usize));
        panic!("enderpy tokens do not match Python tokens.\n{}\n{} token mismatches found", table, num_mismatches);
    }

    fn check_tokens_match(python_token: PythonToken, enderpy_token: Token, lexer: &Lexer) -> Option<TokenMismatch> {
        let kind_matches = match python_token.kind {
            PythonKind::EndMarker => enderpy_token.kind == Kind::Eof,
            PythonKind::Name => enderpy_token.kind == Kind::Identifier,
            PythonKind::Number => matches!(enderpy_token.kind, Kind::Integer | Kind::PointFloat | Kind::ExponentFloat),
            PythonKind::String => enderpy_token.kind == Kind::StringLiteral,
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
            PythonKind::Op => {
                match python_token.value.as_str() {
                    ":" => enderpy_token.kind == Kind::Colon,
                    "=" => enderpy_token.kind == Kind::Assign,
                    "(" => enderpy_token.kind == Kind::LeftParen,
                    ")" => enderpy_token.kind == Kind::RightParen,
                    _ => false,
                }
            },
            PythonKind::Await => enderpy_token.kind == Kind::Await,
            PythonKind::Async => enderpy_token.kind == Kind::Async,
            PythonKind::TypeIgnore => false, // doesn't exist
            PythonKind::TypeComment => false, // doesn't exist
            PythonKind::SoftKeyword => false, // doesn't exist
            PythonKind::FstringStart => enderpy_token.kind == Kind::FStringStart,
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
        let value_matches = match python_token.value.as_str() {
            "\n" => enderpy_token.kind == Kind::NewLine,
            ":" => enderpy_token.kind == Kind::Colon,
            "=" => enderpy_token.kind == Kind::Assign,
            "(" => enderpy_token.kind == Kind::LeftParen,
            ")" => enderpy_token.kind == Kind::RightParen,
            // By default, we expect the values to be the same.
            _ => python_token_value == enderpy_token_value,
        };
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
}
