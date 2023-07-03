use parser::ast::Statement;

use crate::ast_visitor::TraversalVisitor;

struct PreAnalysis;

impl TraversalVisitor for PreAnalysis {}
