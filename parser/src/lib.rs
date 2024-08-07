mod lexer;
mod parser;

pub use crate::{
    lexer::Lexer,
    parser::{ast, parser::Parser},
};
pub mod error;
pub mod runpython;
pub mod token;

pub fn get_row_col_position(start: u32, end: u32, line_starts: &[u32]) -> (u32, u32, u32, u32) {
    let (start_line_num, start_line_offset) = match line_starts.binary_search(&start) {
        Ok(idx) => (idx, line_starts[idx]),
        Err(idx) => (idx - 1, line_starts[idx - 1]),
    };
    let start_line_column = start - start_line_offset;
    // EOF token
    if start == end {
        return (
            start_line_num as u32 + 1,
            start_line_column,
            start_line_num as u32 + 1,
            start_line_column,
        );
    }
    let (end_line_num, end_line_offset) = match line_starts.binary_search(&end) {
        // Special case: this is a new line token
        // When end line offset is exactly on line start it means that this is the new line
        // token end offset. We want to set the new line token line number same for start and
        // end.
        Ok(idx) => (idx - 1, line_starts[idx - 1]),
        Err(idx) => (idx - 1, line_starts[idx - 1]),
    };
    let end_line_column = end.saturating_sub(end_line_offset);

    (
        start_line_num as u32 + 1,
        start_line_column,
        end_line_num as u32 + 1,
        end_line_column,
    )
}
