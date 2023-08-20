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
    pub symbol_table_type: SymbolTableType,
    pub name: String,
    symbols: HashMap<String, SymbolTableNode>,
}

impl SymbolTableScope {
    pub fn new(symbol_table_type: SymbolTableType, name: String) -> Self {
        SymbolTableScope {
            symbol_table_type,
            name,
            symbols: HashMap::new(),
        }
    }
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
    Class(Box<Class>),

    Parameter(Box<Paramter>),

    // TypeParameterDeclaration represents a type parameter in a generic class or function. It models type parameters declared on classes and functions like T in List[T].
    TypeParameter,
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
    pub function_node: ast::FunctionDef,
    pub is_method: bool,
    pub is_generator: bool,
    pub return_statements: Vec<ast::Return>,
    pub yeild_statements: Vec<ast::Yield>,
    // helpful to later type check exceptions
    pub raise_statements: Vec<ast::Raise>,
}

#[derive(Debug)]
pub struct Class {
    pub declaration_path: DeclarationPath,
    // Method names, can be used to look up the function in the symbol table
    // of the class
    pub methods: Vec<String>,
}

#[derive(Debug)]
pub struct Paramter {
    pub declaration_path: DeclarationPath,
    pub parameter_node: ast::Arg,
    pub type_annotation: Option<ast::Expression>,
    pub default_value: Option<ast::Expression>,
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
            symbol_table_type,
            symbols: HashMap::new(),
            name: String::from("global"),
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

    pub fn current_scope_type(&self) -> &SymbolTableType {
        return &self.current_scope().symbol_table_type;
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
