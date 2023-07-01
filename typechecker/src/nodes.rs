// This file holds nodes that represent the AST of a file.
// This holds the same information as the parser AST, but
// in a more convenient format for the type checker.
// NOTE: at this point we're not sure what nodes need to be
// here, so this has the minimum amount of nodes needed to
// get the type checker working. But can be expanded.

use std::{convert, path::PathBuf};

use parser::ast::{Import, ImportFrom, Module, Statement};

use crate::{
    ast_visitor::TraversalVisitor,
    symbol_table::{SymbolTable, SymbolTableType},
};

enum ImportKinds {
    Import(Import),
    ImportFrom(ImportFrom),
}

pub struct EnderpyFile {
    path: PathBuf,
    ast: Module,
    names: SymbolTable,
    // all the imports inside the file
    imports: Vec<ImportKinds>,
    // highlevel definitions inside the file
    defs: Vec<Statement>,
}

impl EnderpyFile {
    pub fn new(path: PathBuf, tree: Module) -> Self {
        let mut converter = ASTConverter::new();
        let mut names = SymbolTable::new(path.to_str(), SymbolTableType::Module, 1);
        converter.visit_module(&tree, &mut names);
        EnderpyFile {
            path,
            ast: tree,
            names,
            defs: converter.defs,
            imports: converter.imports,
        }
    }
}

/// Converts python AST to Enderpy file. This is a high level structure used by
/// the rest of type checker components.
/// This has many reponsibilities and tightly coupled to EnderpyFile.
struct ASTConverter {
    imports: Vec<ImportKinds>,
    defs: Vec<Statement>,
}

impl ASTConverter {
    pub fn new() -> Self {
        return ASTConverter {
            imports: vec![],
            defs: vec![],
        };
    }
}

impl ASTConverter {
    fn convert(&self, tree: Module, path: PathBuf) -> EnderpyFile {
        let mut imports: Vec<ImportKinds> = vec![];
        let mut defs = vec![];
        for stmt in tree.body {
            self.visit_stmt(&stmt);
        }

        EnderpyFile {
            path,
            ast: tree,
            names: SymbolTable::new(path.to_str(), SymbolTableType::Module, 1),
            defs,
            imports,
        }
    }
}

impl TraversalVisitor<ImportKinds> for ASTConverter {
    fn visit_import(&mut self, i: &Import) -> ImportKinds {
        todo!()
    }

    fn visit_import_from(&mut self, i: &ImportFrom) -> ImportKinds {
        todo!()
    }

    fn visit_alias(&mut self, a: &parser::ast::Alias) -> ImportKinds {
        todo!()
    }

    fn visit_assign(&mut self, a: &parser::ast::Assign) -> ImportKinds {
        todo!()
    }

    fn visit_ann_assign(&mut self, a: &parser::ast::AnnAssign) -> ImportKinds {
        todo!()
    }

    fn visit_aug_assign(&mut self, a: &parser::ast::AugAssign) -> ImportKinds {
        todo!()
    }

    fn visit_assert(&mut self, a: &parser::ast::Assert) -> ImportKinds {
        todo!()
    }

    fn visit_pass(&mut self, p: &parser::ast::Pass) -> ImportKinds {
        todo!()
    }

    fn visit_delete(&mut self, d: &parser::ast::Delete) -> ImportKinds {
        todo!()
    }

    fn visit_return(&mut self, r: &parser::ast::Return) -> ImportKinds {
        todo!()
    }

    fn visit_raise(&mut self, r: &parser::ast::Raise) -> ImportKinds {
        todo!()
    }

    fn visit_break(&mut self, b: &parser::ast::Break) -> ImportKinds {
        todo!()
    }

    fn visit_continue(&mut self, c: &parser::ast::Continue) -> ImportKinds {
        todo!()
    }

    fn visit_global(&mut self, g: &parser::ast::Global) -> ImportKinds {
        todo!()
    }

    fn visit_nonlocal(&mut self, n: &parser::ast::Nonlocal) -> ImportKinds {
        todo!()
    }

    fn visit_if(&mut self, i: &parser::ast::If) -> ImportKinds {
        todo!()
    }

    fn visit_while(&mut self, w: &parser::ast::While) -> ImportKinds {
        todo!()
    }

    fn visit_for(&mut self, f: &parser::ast::For) -> ImportKinds {
        todo!()
    }

    fn visit_with(&mut self, w: &parser::ast::With) -> ImportKinds {
        todo!()
    }

    fn visit_try(&mut self, t: &parser::ast::Try) -> ImportKinds {
        todo!()
    }

    fn visit_try_star(&mut self, t: &parser::ast::TryStar) -> ImportKinds {
        todo!()
    }

    fn visit_function_def(&mut self, f: &parser::ast::FunctionDef) -> ImportKinds {
        todo!()
    }

    fn visit_class_def(&mut self, c: &parser::ast::ClassDef) -> ImportKinds {
        todo!()
    }

    fn visit_match(&mut self, m: &parser::ast::Match) -> ImportKinds {
        todo!()
    }
}
