use parser::ast::Statement;

use crate::ast_visitor::FileVisitor;

struct PreAnalysis;

impl FileVisitor<()> for PreAnalysis {
    fn visit_file(&mut self, f: &crate::nodes::EnderpyFile) -> () {
        todo!()
    }
    fn visit_module(&mut self, m: &parser::ast::Module) -> () {
        todo!()
    }
    fn visit_stmt(&mut self, s: &parser::ast::Statement) -> () {
        match s {
            Statement::Import(import) => self.visit_import(import),
            _ => todo!(),
        }
    }

    fn visit_expr(&mut self, e: &parser::ast::Expression) -> () {
        todo!()
    }

    fn visit_import(&mut self, i: &parser::ast::Import) -> () {
        todo!()
    }

    fn visit_import_from(&mut self, i: &parser::ast::ImportFrom) -> () {
        todo!()
    }

    fn visit_alias(&mut self, a: &parser::ast::Alias) -> () {
        todo!()
    }

    fn visit_assign(&mut self, a: &parser::ast::Assign) -> () {
        todo!()
    }

    fn visit_ann_assign(&mut self, a: &parser::ast::AnnAssign) -> () {
        todo!()
    }

    fn visit_aug_assign(&mut self, a: &parser::ast::AugAssign) -> () {
        todo!()
    }

    fn visit_assert(&mut self, a: &parser::ast::Assert) -> () {
        todo!()
    }

    fn visit_pass(&mut self, p: &parser::ast::Pass) -> () {
        todo!()
    }

    fn visit_delete(&mut self, d: &parser::ast::Delete) -> () {
        todo!()
    }

    fn visit_return(&mut self, r: &parser::ast::Return) -> () {
        todo!()
    }

    fn visit_raise(&mut self, r: &parser::ast::Raise) -> () {
        todo!()
    }

    fn visit_break(&mut self, b: &parser::ast::Break) -> () {
        todo!()
    }

    fn visit_continue(&mut self, c: &parser::ast::Continue) -> () {
        todo!()
    }

    fn visit_global(&mut self, g: &parser::ast::Global) -> () {
        todo!()
    }

    fn visit_nonlocal(&mut self, n: &parser::ast::Nonlocal) -> () {
        todo!()
    }

    fn visit_if(&mut self, i: &parser::ast::If) -> () {
        todo!()
    }

    fn visit_while(&mut self, w: &parser::ast::While) -> () {
        todo!()
    }

    fn visit_for(&mut self, f: &parser::ast::For) -> () {
        todo!()
    }

    fn visit_with(&mut self, w: &parser::ast::With) -> () {
        todo!()
    }

    fn visit_try(&mut self, t: &parser::ast::Try) -> () {
        todo!()
    }

    fn visit_try_star(&mut self, t: &parser::ast::TryStar) -> () {
        todo!()
    }

    fn visit_function_def(&mut self, f: &parser::ast::FunctionDef) -> () {
        todo!()
    }

    fn visit_class_def(&mut self, c: &parser::ast::ClassDef) -> () {
        todo!()
    }

    fn visit_match(&mut self, m: &parser::ast::Match) -> () {
        todo!()
    }

    fn visit_constant(&mut self, c: &parser::ast::Constant) -> () {
        todo!()
    }

    fn visit_list(&mut self, l: &parser::ast::List) -> () {
        todo!()
    }

    fn visit_tuple(&mut self, t: &parser::ast::Tuple) -> () {
        todo!()
    }

    fn visit_dict(&mut self, d: &parser::ast::Dict) -> () {
        todo!()
    }

    fn visit_set(&mut self, s: &parser::ast::Set) -> () {
        todo!()
    }

    fn visit_name(&mut self, n: &parser::ast::Name) -> () {
        todo!()
    }

    fn visit_bool_op(&mut self, b: &parser::ast::BoolOperation) -> () {
        todo!()
    }

    fn visit_unary_op(&mut self, u: &parser::ast::UnaryOperation) -> () {
        todo!()
    }

    fn visit_bin_op(&mut self, b: &parser::ast::BinOp) -> () {
        todo!()
    }

    fn visit_named_expr(&mut self, n: &parser::ast::NamedExpression) -> () {
        todo!()
    }

    fn visit_yield(&mut self, y: &parser::ast::Yield) -> () {
        todo!()
    }

    fn visit_yield_from(&mut self, y: &parser::ast::YieldFrom) -> () {
        todo!()
    }

    fn visit_starred(&mut self, s: &parser::ast::Starred) -> () {
        todo!()
    }

    fn visit_generator(&mut self, g: &parser::ast::Generator) -> () {
        todo!()
    }

    fn visit_list_comp(&mut self, l: &parser::ast::ListComp) -> () {
        todo!()
    }

    fn visit_set_comp(&mut self, s: &parser::ast::SetComp) -> () {
        todo!()
    }

    fn visit_dict_comp(&mut self, d: &parser::ast::DictComp) -> () {
        todo!()
    }

    fn visit_attribute(&mut self, a: &parser::ast::Attribute) -> () {
        todo!()
    }

    fn visit_subscript(&mut self, s: &parser::ast::Subscript) -> () {
        todo!()
    }

    fn visit_slice(&mut self, s: &parser::ast::Slice) -> () {
        todo!()
    }

    fn visit_call(&mut self, c: &parser::ast::Call) -> () {
        todo!()
    }

    fn visit_await(&mut self, a: &parser::ast::Await) -> () {
        todo!()
    }

    fn visit_compare(&mut self, c: &parser::ast::Compare) -> () {
        todo!()
    }

    fn visit_lambda(&mut self, l: &parser::ast::Lambda) -> () {
        todo!()
    }

    fn visit_if_exp(&mut self, i: &parser::ast::IfExp) -> () {
        todo!()
    }

    fn visit_joined_str(&mut self, j: &parser::ast::JoinedStr) -> () {
        todo!()
    }

    fn visit_formatted_value(&mut self, f: &parser::ast::FormattedValue) -> () {
        todo!()
    }
}
