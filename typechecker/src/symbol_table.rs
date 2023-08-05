use parser::ast::{self, Node};
use std::collections::HashMap;

#[derive(Debug)]
pub struct SymbolTable {
    // Sub tables are scopes inside the current scope
    scopes: Vec<SymbolTableScope>,
    // When a symbol goes out of scope we save it here to be able to look it up later
    all_scopes: Vec<SymbolTableScope>,
}

#[derive(Debug)]
pub struct SymbolTableScope {
    pub start_line_number: u8,
    pub symbol_table_type: SymbolTableType,
    symbols: HashMap<String, SymbolTableNode>,
}

#[derive(Debug)]
pub enum SymbolTableType {
    Module,
    Class,
    Function,
}

#[derive(Debug)]
pub struct SymbolTableNode {
    pub name: String,
    pub declarations: Vec<Declaration>,
    pub module_public: bool,
    pub module_hidden: bool,
    pub implicit: bool,
    pub scope: SymbolScope,
}

#[derive(Debug, Clone)]
pub struct DeclarationPath {
    pub module_name: String,
    pub node: Node,
}

#[derive(Debug)]
pub enum Declaration {
    Variable(Box<Variable>),
    Function(Box<Function>),
}

#[derive(Debug)]
pub struct Variable {
    pub declaration_path: DeclarationPath,
    pub scope: SymbolScope,
    pub type_annotation: Option<ast::Expression>,
    pub inferred_type_source: Option<ast::Expression>,
    pub is_constant: bool,
}

#[derive(Debug)]
pub struct Function {
    pub declaration_path: DeclarationPath,
    pub scope: SymbolScope,
    pub is_method: bool,
    pub is_generator: bool,
    pub return_statements: Vec<ast::Return>,
    pub yeild_statements: Vec<ast::Yield>,
    // helpful to later type check exceptions
    pub raise_statements: Vec<ast::Raise>,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolScope {
    Global,
    Nonlocal,
    Local,
    Unknown,
}

impl SymbolTable {
    pub fn new(symbol_table_type: SymbolTableType, start_line_number: u8) -> Self {
        let global_scope = SymbolTableScope {
            start_line_number,
            symbol_table_type,
            symbols: HashMap::new(),
        };
        SymbolTable {
            scopes: vec![global_scope],
            all_scopes: vec![],
        }
    }

    fn current_scope(&self) -> &SymbolTableScope {
        if let Some(scope) = self.scopes.last() {
            return &scope;
        } else {
            panic!("no scopes")
        }
    }
    pub fn lookup_in_scope(&self, name: &str) -> Option<&SymbolTableNode> {
        let cur_scope = self.current_scope();
        return cur_scope.symbols.get(name);
    }

    pub fn enter_scope(&mut self, new_scope: SymbolTableScope) {
        self.scopes.push(new_scope);
    }

    pub fn exit_scope(&mut self) {
        let finished_scope = self.scopes.pop();
        match finished_scope {
            Some(scope) => self.all_scopes.push(scope),
            None => panic!("tried to exit non-existent scope"),
        }
    }

    pub fn add_symbol(&mut self, symbol_node: SymbolTableNode) {
        match self.scopes.last_mut() {
            Some(scope) => {
                scope.symbols.insert(symbol_node.name.clone(), symbol_node);
            }
            None => panic!("no current scope, there must be a global scope"),
        };
    }
}

impl SymbolTableNode {
    pub fn add_declaration(&mut self, decl: Declaration) {
        self.declarations.push(decl);
    }
}
