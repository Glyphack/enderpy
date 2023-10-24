use enderpy_python_parser::ast::{self, Node};
use std::{collections::HashMap, fmt::Display};

use crate::{
    nodes::ImportKinds,
    ruff_python_import_resolver::{
        import_result::ImportResult, module_descriptor::ImportModuleDescriptor,
    },
};

#[derive(Debug, Clone)]
pub struct SymbolTable {
    // Sub tables are scopes inside the current scope
    scopes: Vec<SymbolTableScope>,
    // When a symbol goes out of scope we save it here to be able to look it up later
    all_scopes: Vec<SymbolTableScope>,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum SymbolTableType {
    Module,
    Class,
    Function,
}

#[derive(Debug, Clone)]
pub struct SymbolTableNode {
    pub name: String,
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub struct DeclarationPath {
    pub module_name: String,
    pub node: Node,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Variable(Box<Variable>),
    Function(Box<Function>),
    Class(Box<Class>),

    // Alias is used for imports
    Alias(Box<Alias>),

    Parameter(Box<Paramter>),
    // TypeParameterDeclaration represents a type parameter in a generic class or function. It models type parameters declared on classes and functions like T in List[T].
}

impl Declaration {
    pub fn declaration_path(&self) -> &DeclarationPath {
        match self {
            Declaration::Variable(v) => &v.declaration_path,
            Declaration::Function(f) => &f.declaration_path,
            Declaration::Class(c) => &c.declaration_path,
            Declaration::Parameter(p) => &p.declaration_path,
            Declaration::Alias(a) => &a.declaration_path,
        }
    }
}

impl Display for DeclarationPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{:?}", self.module_name, self.node)
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub declaration_path: DeclarationPath,
    pub scope: SymbolScope,
    pub type_annotation: Option<ast::Expression>,
    pub inferred_type_source: Option<ast::Expression>,
    pub is_constant: bool,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Class {
    pub declaration_path: DeclarationPath,
    // Method names, can be used to look up the function in the symbol table
    // of the class
    pub methods: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Paramter {
    pub declaration_path: DeclarationPath,
    pub parameter_node: ast::Arg,
    pub type_annotation: Option<ast::Expression>,
    pub default_value: Option<ast::Expression>,
}

#[derive(Debug, Clone)]
pub struct Alias {
    pub declaration_path: DeclarationPath,
    /// The import node that this alias is for. Only one of import_node or import_from_node will be set
    pub import_from_node: Option<ast::ImportFrom>,
    pub import_node: Option<ast::Import>,
    /// Name of the imported symbol in case of ImportFrom
    /// e.g. From bar import baz -> baz is the symbol name
    pub symbol_name: Option<String>,
    /// The result of the import
    pub import_result: ImportResult,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolScope {
    Global,
    Nonlocal,
    Local,
    Unknown,
}

impl SymbolTable {
    pub fn new(symbol_table_type: SymbolTableType, _start_line_number: u8) -> Self {
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
            scope
        } else {
            panic!("no scopes")
        }
    }

    pub fn current_scope_type(&self) -> &SymbolTableType {
        return &self.current_scope().symbol_table_type;
    }

    // TODO: have a way to look up in the parent scopes as well
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

    pub fn add_symbol(&mut self, mut symbol_node: SymbolTableNode) {
        match self.scopes.last_mut() {
            Some(scope) => {
                if let Some(existing_symbol) = scope.symbols.get(&symbol_node.name) {
                    symbol_node
                        .declarations
                        .extend(existing_symbol.declarations.clone());
                }
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

    pub fn last_declaration(&self) -> Option<&Declaration> {
        self.declarations.last()
    }

    pub fn declaration_until_position(&self, position: usize) -> Option<&Declaration> {
        let mut filtered_declarations = self
            .declarations
            .iter()
            .filter(|decl| decl.declaration_path().node.start < position)
            .collect::<Vec<&Declaration>>();

        filtered_declarations.sort_by(|a, b| {
            a.declaration_path()
                .node
                .start
                .cmp(&b.declaration_path().node.start)
        });

        filtered_declarations.last().copied()
    }
}

// implement display for symbol table and sort the symbols by key

impl std::fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "-------------------")?;
        writeln!(f, "global scope:")?;
        let mut sorted_scopes = self.scopes.iter().collect::<Vec<&SymbolTableScope>>();
        sorted_scopes.sort_by(|a, b| a.name.cmp(&b.name));

        for scope in sorted_scopes {
            writeln!(f, "{}", scope)?;
        }

        writeln!(f, "all scopes:")?;

        let mut sorted_all_scopes = self.all_scopes.iter().collect::<Vec<&SymbolTableScope>>();
        sorted_all_scopes.sort_by(|a, b| a.name.cmp(&b.name));
        for scope in sorted_all_scopes {
            writeln!(f, "{}", scope)?;
        }
        writeln!(f, "-------------------")?;
        Ok(())
    }
}

impl std::fmt::Display for SymbolTableScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut sorted_symbols = self
            .symbols
            .iter()
            .collect::<Vec<(&String, &SymbolTableNode)>>();
        sorted_symbols.sort_by(|a, b| a.0.cmp(b.0));

        writeln!(f, "Symbols:")?;
        for (name, symbol) in sorted_symbols {
            writeln!(f, "{}", name)?;
            // sort the declarations by line number
            let mut sorted_declarations = symbol.declarations.clone();
            sorted_declarations.sort_by(|a, b| {
                a.declaration_path()
                    .node
                    .start
                    .cmp(&b.declaration_path().node.start)
            });

            writeln!(f, "- Declarations:")?;

            for declaration in sorted_declarations {
                writeln!(f, "--:   {}", declaration)?;
            }
        }
        Ok(())
    }
}

impl std::fmt::Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaration::Variable(v) => write!(f, "{:#?}", v),
            Declaration::Function(fun) => write!(f, "{:#?}", fun),
            Declaration::Class(c) => write!(f, "{:#?}", c),
            Declaration::Parameter(p) => write!(f, "{:#?}", p),
            Declaration::Alias(a) => write!(f, "{:#?}", a),
        }
    }
}
