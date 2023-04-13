use crate::token::Kind;

pub fn get_fstring_token(c: &char, fstring_starter: &String) -> Option<Kind> {
    if *c == '{' {
        return Some(Kind::FStringMiddle);
    } else if *fstring_starter == *c.to_string() {
        return Some(Kind::FStringEnd);
    }

    None
}
