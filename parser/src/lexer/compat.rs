use crate::token::{Token, Kind};
use crate::lexer::Lexer;

#[allow(dead_code)]
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
    TypeIgnore,
    TypeComment,
    SoftKeyword,
    FstringStart,
    FstringMiddle,
    FstringEnd,
    Comment,
    NL,
    ErrorToken,
    Encoding,
    NTokens,
    NTOffset,
    // Special type for conversion.
    Unknown,
}

impl PythonKind {
    pub fn name(&self) -> String {
        match self {
            PythonKind::TypeIgnore => "TYPE_IGNORE".into(),
            PythonKind::TypeComment => "TYPE_COMMENT".into(),
            PythonKind::SoftKeyword => "SOFT_KEYWORD".into(),
            PythonKind::FstringStart => "FSTRING_START".into(),
            PythonKind::FstringMiddle => "FSTRING_MIDDLE".into(),
            PythonKind::FstringEnd => "FSTRING_END".into(),
            PythonKind::NTokens => "N_TOKENS".into(),
            PythonKind::NTOffset => "NT_OFFSET".into(),
            _ => self.name().to_uppercase(),
        }
    }
}

impl From<Kind> for PythonKind {
    fn from(kind: Kind) -> Self {
        match kind {
            Kind::NewLine => PythonKind::NewLine,
            Kind::Indent => PythonKind::Indent,
            Kind::Dedent => PythonKind::Dedent,
            Kind::Identifier => PythonKind::Unknown,
            Kind::False => PythonKind::Unknown,
            Kind::None => PythonKind::Unknown,
            Kind::True => PythonKind::Unknown,
            Kind::And => PythonKind::Unknown,
            Kind::As => PythonKind::Unknown,
            Kind::Assert => PythonKind::Unknown,
            Kind::Async => PythonKind::Async,
            Kind::Await => PythonKind::Await,
            Kind::Break => PythonKind::Unknown,
            Kind::Class => PythonKind::Unknown,
            Kind::Continue => PythonKind::Unknown,
            Kind::Comment => PythonKind::Comment,
            Kind::Def => PythonKind::Unknown,
            Kind::Del => PythonKind::Unknown,
            Kind::Elif => PythonKind::Unknown,
            Kind::Else => PythonKind::Unknown,
            Kind::Ellipsis => PythonKind::Ellipsis,
            Kind::Except => PythonKind::Unknown,
            Kind::Finally => PythonKind::Unknown,
            Kind::For => PythonKind::Unknown,
            Kind::From => PythonKind::Unknown,
            Kind::Global => PythonKind::Unknown,
            Kind::If => PythonKind::Unknown,
            Kind::Import => PythonKind::Unknown,
            Kind::In => PythonKind::Unknown,
            Kind::Is => PythonKind::Unknown,
            Kind::Lambda => PythonKind::Unknown,
            Kind::Nonlocal => PythonKind::Unknown,
            Kind::Not => PythonKind::Unknown,
            Kind::Or => PythonKind::Unknown,
            Kind::Pass => PythonKind::Unknown,
            Kind::Raise => PythonKind::Unknown,
            Kind::Return => PythonKind::Unknown,
            Kind::Try => PythonKind::Unknown,
            Kind::While => PythonKind::Unknown,
            Kind::With => PythonKind::Unknown,
            Kind::Yield => PythonKind::Unknown,
            Kind::StringLiteral => PythonKind::Unknown,
            Kind::FStringStart => PythonKind::FstringStart,
            Kind::FStringMiddle => PythonKind::FstringMiddle,
            Kind::FStringEnd => PythonKind::FstringEnd,
            Kind::Bytes => PythonKind::Unknown,
            Kind::RawFStringStart => PythonKind::Unknown,
            Kind::RawBytes => PythonKind::Unknown,
            Kind::Unicode => PythonKind::Unknown,
            Kind::Integer => PythonKind::Number,
            Kind::Binary => PythonKind::Unknown,
            Kind::Octal => PythonKind::Unknown,
            Kind::Hexadecimal => PythonKind::Unknown,
            Kind::PointFloat => PythonKind::Number,
            Kind::ExponentFloat => PythonKind::Number,
            Kind::ImaginaryInteger => PythonKind::Unknown,
            Kind::ImaginaryPointFloat => PythonKind::Unknown,
            Kind::ImaginaryExponentFloat => PythonKind::Unknown,
            Kind::Plus => PythonKind::Plus,
            Kind::Minus => PythonKind::Minus,
            Kind::Mul => PythonKind::Star,
            Kind::Pow => PythonKind::DoubleStar,
            Kind::Div => PythonKind::Slash,
            Kind::IntDiv => PythonKind::DoubleSlash,
            Kind::Mod => PythonKind::Percent,
            Kind::MatrixMul => PythonKind::At,
            Kind::LeftShift => PythonKind::LeftShift,
            Kind::RightShift => PythonKind::RightShift,
            Kind::BitAnd => PythonKind::Amper,
            Kind::BitOr => PythonKind::VBar,
            Kind::BitXor => PythonKind::Circumflex,
            Kind::BitNot => PythonKind::Tilde,
            Kind::Walrus => PythonKind::ColonEqual,
            Kind::Less => PythonKind::Less,
            Kind::Greater => PythonKind::Greater,
            Kind::LessEq => PythonKind::LessEqual,
            Kind::GreaterEq => PythonKind::GreaterEqual,
            Kind::Eq => PythonKind::Equal,
            Kind::NotEq => PythonKind::NotEqual,
            Kind::LeftParen => PythonKind::LPar,
            Kind::RightParen => PythonKind::RPar,
            Kind::LeftBrace => PythonKind::LSqb,
            Kind::RightBrace => PythonKind::RSqb,
            Kind::LeftBracket => PythonKind::LBrace,
            Kind::RightBracket => PythonKind::RBrace,
            Kind::Comma => PythonKind::Comma,
            Kind::Colon => PythonKind::Colon,
            Kind::Dot => PythonKind::Dot,
            Kind::SemiColon => PythonKind::Semi,
            Kind::Assign => PythonKind::Equal,
            Kind::Arrow => PythonKind::RArrow,
            Kind::AddAssign => PythonKind::PlusEqual,
            Kind::SubAssign => PythonKind::MinEqual,
            Kind::MulAssign => PythonKind::StarEqual,
            Kind::DivAssign => PythonKind::SlashEqual,
            Kind::ModAssign => PythonKind::PercentEqual,
            Kind::MatrixMulAssign => PythonKind::AtEqual,
            Kind::BitAndAssign => PythonKind::AmperEqual,
            Kind::BitOrAssign => PythonKind::VBarEqual,
            Kind::BitXorAssign => PythonKind::CircumflexEqual,
            Kind::IntDivAssign => PythonKind::DoubleSlashEqual,
            Kind::ShiftLeftAssign => PythonKind::LeftShiftEqual,
            Kind::ShiftRightAssign => PythonKind::RightShiftEqual,
            Kind::PowAssign => PythonKind::DoubleStarEqual,
            Kind::BackSlash => PythonKind::Unknown,
            Kind::Dollar => PythonKind::Unknown,
            Kind::QuestionMark => PythonKind::Unknown,
            Kind::BackTick => PythonKind::Unknown,
            Kind::WhiteSpace => PythonKind::Unknown,
            Kind::Error => PythonKind::ErrorToken,
            Kind::Eof => PythonKind::EndMarker,
        }
    }
}

pub struct PythonToken {
    kind: PythonKind,
    value: String,
    start: (u32, u32),
    end: (u32, u32),
}

pub fn pythonize_token(token: Token, lexer: Lexer) -> PythonToken {
    PythonToken {
        kind: PythonKind::from(token.kind),
        value: token.value.to_string(),
        start: lexer.to_row_col(token.start),
        end: lexer.to_row_col(token.end),
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_lexer() {
    }
}
