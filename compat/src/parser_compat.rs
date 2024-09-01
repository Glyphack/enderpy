#![allow(clippy::all)]
use assert_json_diff::assert_json_matches_no_panic;
use miette::{bail, IntoDiagnostic, Result};
use serde_json::Value;
use std::convert::From;
use std::io::Write;

use crate::runpython::{default_python_path, spawn_python_script_command};
use enderpy_python_parser::parser::compat::AsPythonCompat;
use enderpy_python_parser::Parser;
use tabled::{
    builder::Builder,
    settings::peaker::PriorityMax,
    settings::{Style, Width},
};

use terminal_size::{terminal_size, Width as TerminalWidth};

fn parse_python_source(source: &str) -> Result<Value> {
    let mut process = spawn_python_script_command(
        "compat/ast_python.py",
        vec!["--stdin"],
        default_python_path()?,
    )?;

    // Get process stdin and write the input string.
    if let Some(mut stdin) = process.stdin.take() {
        stdin.write_all(source.as_bytes()).into_diagnostic()?;
    } else {
        bail!("Failed to open stdin when running `compat/ast_python.py`");
    }
    // Get process stdout and parse result.
    let output = process.wait_with_output().into_diagnostic()?;
    let mut ast =
        serde_json::from_str(String::from_utf8_lossy(&output.stdout).as_ref()).into_diagnostic()?;
    remove_unimplemented_attributes(&mut ast);
    Ok(ast)
}
pub fn python_parser_test_ast(inputs: &[&str]) {
    for test_input in inputs.iter() {
        let enderpy_ast = parse_enderpy_source(test_input).unwrap();
        let python_ast = parse_python_source(test_input).unwrap();
        assert_ast_eq(&python_ast, &enderpy_ast, test_input);
    }
}

fn assert_ast_eq(python_ast: &Value, enderpy_ast: &Value, source: &str) {
    let include_source = std::env::var("INCLUDE_SOURCE").is_ok();
    let side_by_side = std::env::var("SIDE_BY_SIDE").is_ok();

    let formatted_source = if include_source {
        format!("\nSource:\n{}\n", source)
    } else {
        "".to_string()
    };
    if !side_by_side {
        pretty_assertions::assert_eq!(
                &python_ast,
                &enderpy_ast,
                "Enderpy AST does not match Python AST.\n{}\x1b[31mPython AST\x1b[0m / \x1b[32mEnderpy AST\x1b[0m",
                formatted_source,
            );
    } else if let Err(message) = assert_json_matches_no_panic(
        &python_ast,
        &enderpy_ast,
        assert_json_diff::Config::new(assert_json_diff::CompareMode::Strict),
    ) {
        let mut table_builder = Builder::default();
        table_builder.push_record(["Python AST", "Enderpy AST"]);
        table_builder.push_record([
            serde_json::to_string_pretty(&python_ast).unwrap(),
            serde_json::to_string_pretty(&enderpy_ast).unwrap(),
        ]);
        let mut table = table_builder.build();
        table.with(Style::modern());
        // If run in a terminal, don't expand table beyond terminal width.
        if let Some((TerminalWidth(width), _)) = terminal_size() {
            table
                .with(
                    Width::wrap(width as usize)
                        .keep_words()
                        .priority::<PriorityMax>(),
                )
                .with(Width::increase(width as usize));
        }
        panic!(
            "Enderpy AST does not match Python AST.\n{}{}\n{}",
            formatted_source, table, message
        );
    }
}
fn remove_unimplemented_attributes(value: &mut Value) {
    match value {
        Value::Object(map) => {
            // TODO ast_python: Adjust these ignored values as Enderpy adds support.
            map.retain(|key, _| !matches!(key.as_str(), "ctx" | "type_ignores" | "kind"));
            for (_, v) in map.iter_mut() {
                remove_unimplemented_attributes(v);
            }
        }
        Value::Array(vec) => {
            for v in vec.iter_mut() {
                remove_unimplemented_attributes(v);
            }
        }
        _ => {
            // Nothing to do for other value types.
        }
    };
}

fn parse_enderpy_source(source: &str) -> Result<Value> {
    let mut parser = Parser::new(source, "string");
    let typed_ast = parser.parse().into_diagnostic()?;
    let ast = typed_ast.as_python_compat(&parser);
    Ok(ast)
}

#[allow(unused_macros)]
macro_rules! parser_test {
    ($test_name:ident, $test_file:expr) => {
        #[test]
        fn $test_name() {
            let test_case = std::fs::read_to_string($test_file).unwrap();
            python_parser_test_ast(&[test_case.as_str()]);
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_compat() {
        //         let source = r#"
        // def x(a: int) -> int:
        //     return 1 + 1
        // b = x(1)
        // print(b)
        // "#;

        let source = r#"(a
, b, c)
"#;

        let enderpy_ast = parse_enderpy_source(source).unwrap();
        let python_ast = parse_python_source(source).unwrap();
        assert_ast_eq(&python_ast, &enderpy_ast, source);
    }

    #[test]
    fn test_parse_assignment() {
        python_parser_test_ast(&[
            "a = 1",
            "a = None",
            "a = True",
            "a = False",
            "a = 1j",
            // TODO ast_python: Python does not evaluate bytes.
            // "a = b'1'",
            // "a = rb'1'",
            // "a = br'1'",
            "a = \"a\"",
            "a = '''a'''",
            "a = \"\"\"a\"\"\"",
            "a = 'a'",
            "a = 1, 2",
            "a = 1, 2, ",
            "a = b = 1",
            "a,b = c,d = 1,2",
            // augmented assignment
            "a += 1",
            "a -= 1",
            "a *= 1",
            "a /= 1",
            "a //= 1",
            "a %= 1",
            "a **= 1",
            "a <<= 1",
            "a >>= 1",
            "a &= 1",
            "a ^= 1",
            "a |= 1",
            // annotated assignment
        ]);
    }

    #[test]
    fn test_parse_assert_stmt() {
        python_parser_test_ast(&["assert a", "assert a, b", "assert True, 'fancy message'"]);
    }

    #[test]
    fn test_pass_stmt() {
        python_parser_test_ast(&["pass", "pass ", "pass\n"]);
    }

    #[test]
    fn test_parse_del_stmt() {
        python_parser_test_ast(&["del a", "del a, b", "del a, b, "]);
    }

    #[test]
    fn parse_yield_statement() {
        python_parser_test_ast(&["yield", "yield a", "yield a, b", "yield a, b, "]);
    }

    #[test]
    fn test_raise_statement() {
        python_parser_test_ast(&["raise", "raise a", "raise a from c"]);
    }

    #[test]
    fn test_parse_break_continue() {
        python_parser_test_ast(&["break", "continue"]);
    }

    #[test]
    fn test_parse_bool_op() {
        python_parser_test_ast(&[
            "a or b",
            "a and b",
            // TODO: Python parses this as a BoolOp with 3 values.
            // i.e. {"op": "or", "values": ["a", "b", "c"]}
            // Enderpy parses this as a nested set of BoolOps.
            // i.e. {"op": "or", "values": ["a", {"op": "or", "values": ["b", "c"]}]}
            // "a or b or c",
            "a and b or c",
        ]);
    }

    #[test]
    fn test_parse_unary_op() {
        python_parser_test_ast(&["not a", "+ a", "~ a", "-a"]);
    }

    #[test]
    fn test_named_expression() {
        python_parser_test_ast(&["(a := b)"]);
    }

    #[test]
    fn test_tuple() {
        python_parser_test_ast(&[
            "(a, b, c)",
            "(a,
            b, c)",
            "(a
            , b, c)",
            "(a,
            b,
                c)",
            "(a,
            )",
            "(a, b, c,)",
        ]);
    }

    #[test]
    fn test_yield_expression() {
        python_parser_test_ast(&["yield", "yield a", "yield from a"]);
    }

    #[test]
    fn test_await_expression() {
        python_parser_test_ast(&["await a"]);
    }

    #[test]
    fn test_attribute_ref() {
        python_parser_test_ast(&["a.b", "a.b.c", "a.b_c", "a.b.c.d"]);
    }
    #[test]
    fn test_subscript() {
        python_parser_test_ast(&["a[1]", "a.b[1]"]);
    }

    #[test]
    fn parse_call() {
        python_parser_test_ast(&[
            "a()",
            "a(b)",
            "a(b, c)",
            "func(b=c)",
            "func(a, b=c, d=e)",
            "func(a, b=c, d=e, *f)",
            "func(a, b=c, d=e, *f, **g)",
            "func(a,)",
        ]);
    }

    #[test]
    fn test_lambda() {
        python_parser_test_ast(&[
            "lambda: a",
            "lambda a: a",
            "lambda a, b: a",
            "lambda a, b, c: a",
            "lambda a, *b: a",
            "lambda a, *b, c: a",
            "lambda a, *b, c, **d: a",
            "lambda a=1 : a",
            "lambda a=1 : a,",
        ]);
    }

    #[test]
    fn test_conditional_expression() {
        python_parser_test_ast(&["a if b else c if d else e"]);
    }

    #[test]
    fn test_string_literal_concatenation() {
        python_parser_test_ast(&[
            "'a' 'b'",
            // TODO ast_python: Python evaluates this as "ab".
            // "b'a' b'b'",
            "'a'   'b'",
            // TODO ast_python: Enderpy evaluates this as 'r"a"b'. This seems wrong.
            // "r'a' 'b'",
            "('a'
            'b')",
            "('a'
            'b', 'c')",
            "('a'
                            'b'
            'c')",
            // TODO ast_python: Python evaluates this as "ac". Enderpy creates 2 constants.
            // "f'a' 'c'",
            // TODO ast_python: Python evaluates this as "abc". Enderpy creates 3 constants.
            // "f'a' 'b' 'c'",
            // TODO ast_python: Python evaluates this as "dab". Enderpy creates 3 constants.
            // "'d' f'a' 'b'",
            "f'a_{1}' 'b' ",
        ]);
    }

    #[test]
    fn test_fstring() {
        python_parser_test_ast(&[
            "f'a'",
            "f'hello_{a}'",
            "f'hello_{a} {b}'",
            "f'hello_{a} {b} {c}'",
            "f'hello_{f'''{a}'''}'",
        ]);
    }

    #[test]
    fn test_comparison() {
        python_parser_test_ast(&[
            "a == b",
            "a != b",
            "a > b",
            "a < b",
            "a >= b",
            "a <= b",
            "a is b",
            "a is not b",
            "a in b",
            "a not in b",
            "a < b < c",
        ]);
    }

    #[test]
    fn test_while_statement() {
        python_parser_test_ast(&[
            "while a: pass",
            "while a:
    pass",
            "while a:
        a = 1
else:
        b = 1
",
        ]);
    }

    #[test]
    fn test_try_statement() {
        python_parser_test_ast(&[
            "try:
    pass
except:
    pass",
            "try:
                pass
except Exception:
                pass",
            "try:
                pass
except Exception as e:
                pass",
            "try:
                pass
except Exception as e:
                pass
else:
                pass",
            "try:
                pass
except Exception as e:
                pass
else:
                pass
finally:
                pass",
            "try:
    pass
except *Exception as e:
    pass
",
        ]);
    }

    #[test]
    fn test_ellipsis_statement() {
        python_parser_test_ast(&[
            "def a(): ...",
            "def a():
    ...",
            "a = ...",
            "... + 1",
        ]);
    }

    parser_test!(test_functions, "../parser/test_data/inputs/functions.py");
    parser_test!(test_if, "../parser/test_data/inputs/if.py");
    parser_test!(
        test_indentation,
        "../parser/test_data/inputs/indentation.py"
    );
    parser_test!(
        test_separate_statements,
        "../parser/test_data/inputs/separate_statements.py"
    );
    // parser_test!(test_try, "../parser/test_data/inputs/try.py");
    // parser_test!(
    //     annotated_assignment,
    //     "../parser/test_data/inputs/annotated_assignment.py"
    // );
    parser_test!(binary_op, "../parser/test_data/inputs/binary_op.py");
    parser_test!(class, "../parser/test_data/inputs/class.py");
    // parser_test!(dict, "../parser/test_data/inputs/dict.py");
    // parser_test!(test_for, "../parser/test_data/inputs/for.py");
    parser_test!(from_import, "../parser/test_data/inputs/from_import.py");
    parser_test!(function_def, "../parser/test_data/inputs/function_def.py");
    // parser_test!(
    //     generator_expressions,
    //     "../parser/test_data/inputs/generator_expressions.py"
    // );
    // parser_test!(lists, "../parser/test_data/inputs/lists.py");
    // parser_test!(test_match, "../parser/test_data/inputs/match.py");
    // parser_test!(sets, "../parser/test_data/inputs/sets.py");
    // parser_test!(string, "../parser/test_data/inputs/string.py");
    // parser_test!(subscript, "../parser/test_data/inputs/subscript.py");
    // parser_test!(with, "../parser/test_data/inputs/with.py");
    // parser_test!(newlines, "../parser/test_data/inputs/newlines.py");
    parser_test!(comments, "../parser/test_data/inputs/comments.py");
    // parser_test!(types_alias, "../parser/test_data/inputs/type_alias.py");
}
