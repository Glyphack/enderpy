use bitflags::bitflags;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fmt::Display, path::PathBuf};

use enderpy_python_parser::ast::{self, ClassDef, FunctionDef, Node};

use crate::ruff_python_import_resolver::import_result::ImportResult;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    // Sub tables are scopes inside the current scope
    // after building symbol table is finished this only contains the most outer scope
    scopes: Vec<SymbolTableScope>,

    prev_scope: Option<usize>,
    pub current_scope_id: usize,

    /// Name of the module that this symbol table is for
    pub module_name: String,
    pub file_path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct SymbolTableScope {
    pub id: usize,
    pub start_pos: usize,
    pub kind: SymbolTableType,
    pub name: String,
    symbols: HashMap<String, SymbolTableNode>,
    parent: Option<usize>,
}

fn get_id() -> usize {
    use std::sync::atomic::{AtomicUsize, Ordering};
    static COUNTER: AtomicUsize = AtomicUsize::new(1);
    COUNTER.fetch_add(1, Ordering::SeqCst)
}

impl SymbolTableScope {
    pub fn new(
        symbol_table_type: SymbolTableType,
        name: String,
        start_line_number: usize,
        parent: usize,
    ) -> Self {
        SymbolTableScope {
            id: get_id(),
            kind: symbol_table_type,
            name,
            symbols: HashMap::new(),
            parent: Some(parent),
            start_pos: start_line_number,
        }
    }

    pub fn global_scope() -> Self {
        SymbolTableScope {
            id: 0,
            kind: SymbolTableType::Module,
            name: String::from("global"),
            symbols: HashMap::new(),
            parent: None,
            start_pos: 0,
        }
    }
}

#[derive(Debug, Clone, is_macro::Is)]
#[allow(clippy::upper_case_acronyms)]
/// Does not include PEP 695: https://peps.python.org/pep-0695/#scoping-behavior
pub enum SymbolTableType {
    /// BUILTIN scope is used for builtins like len, print, etc.
    BUILTIN,
    Module,
    Class(ClassDef),
    Function(FunctionDef),
}

bitflags! {
    #[repr(transparent)]
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub struct SymbolFlags: u16 {
        const CLASS_MEMBER = 1 << 0;

        const INSTANCE_MEMBER = 1 << 1;

    }
}

#[derive(Debug, Clone)]
pub struct SymbolTableNode {
    pub name: String,
    pub declarations: Vec<Declaration>,
    pub flags: SymbolFlags,
}

#[derive(Debug, Clone)]
pub struct DeclarationPath {
    pub module_name: PathBuf,
    pub node: Node,
}

#[derive(Debug, Clone, is_macro::Is)]
pub enum Declaration {
    Variable(Variable),
    Function(Function),
    Class(Class),

    // Alias is used for imports
    Alias(Alias),

    Parameter(Parameter),
    // TypeParameterDeclaration represents a type parameter in a generic class or function.
    // It models type parameters declared on classes and functions like T in List[T].
    TypeParameter(TypeParameter),

    TypeAlias(TypeAlias),
}

impl Declaration {
    pub fn declaration_path(&self) -> &DeclarationPath {
        match self {
            Declaration::Variable(v) => &v.declaration_path,
            Declaration::Function(f) => &f.declaration_path,
            Declaration::Class(c) => &c.declaration_path,
            Declaration::Parameter(p) => &p.declaration_path,
            Declaration::Alias(a) => &a.declaration_path,
            Declaration::TypeParameter(t) => &t.declaration_path,
            Declaration::TypeAlias(t) => &t.declaration_path,
        }
    }

    pub fn is_in_typeshed(&self) -> bool {
        self.declaration_path().module_name.components().any(|c| {
            c.as_os_str()
                .to_str()
                .map(|s| s.starts_with("typeshed"))
                .unwrap_or(false)
        })
    }
}

impl Display for DeclarationPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}:{:?}", self.module_name, self.node)
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
    /// return statements that are reachable in the top level function body
    pub return_statements: Vec<ast::Return>,
    /// yield statements that are reachable in the top level function body
    pub yield_statements: Vec<ast::Yield>,
    /// raise statements that are reachable in the top level function body
    pub raise_statements: Vec<ast::Raise>,
}

impl Function {
    pub fn is_abstract(&self) -> bool {
        if !self.is_method {
            return false;
        }
        for decorator in self.function_node.decorator_list.iter() {
            if let ast::Expression::Name(n) = &decorator {
                if &n.id == "abstractmethod" {
                    return true;
                }
            }
        }
        false
    }
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub declaration_path: DeclarationPath,
    // Method names, can be used to look up the function in the symbol table
    // of the class
    pub methods: Vec<String>,
    // instance attributes that are defined in the __init__ method
    // if the attribute is referencing another symbol we need to look up that symbol in the
    // __init__ method
    pub attributes: HashMap<String, ast::Expression>,

    // Special classes are classes that are _SpecialForm in typeshed.
    // These classes have their behavior defined in PEPs so we need to handle them differently
    pub special: bool,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub declaration_path: DeclarationPath,
    pub parameter_node: ast::Arg,
    pub type_annotation: Option<ast::Expression>,
    pub default_value: Option<ast::Expression>,
}

#[derive(Debug, Clone)]
pub struct TypeParameter {
    pub declaration_path: DeclarationPath,
    pub type_parameter_node: ast::TypeParam,
}

#[derive(Debug, Clone)]
pub struct Alias {
    pub declaration_path: DeclarationPath,
    /// The import node that this alias is for. Only one of import_node or
    /// import_from_node will be set
    pub import_from_node: Option<ast::ImportFrom>,
    pub import_node: Option<ast::Import>,
    /// Name of the imported symbol in case of ImportFrom
    /// e.g. From bar import baz -> baz is the symbol name
    pub symbol_name: Option<String>,
    /// The result of the import
    pub import_result: ImportResult,
}

#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub declaration_path: DeclarationPath,
    pub type_alias_node: ast::TypeAlias,
}

pub struct LookupSymbolRequest {
    pub name: String,
    pub position: Option<usize>,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolScope {
    Global,
    Nonlocal,
    Local,
    Unknown,
}

impl SymbolTable {
    pub fn new(module_name: String, file_path: PathBuf) -> Self {
        SymbolTable {
            scopes: vec![SymbolTableScope::global_scope()],
            current_scope_id: 0,
            prev_scope: None,
            module_name,
            file_path,
        }
    }

    /// Do not use for lookup operations
    pub fn current_scope(&self) -> &SymbolTableScope {
        if let Some(scope) = self
            .scopes
            .iter()
            .filter(|scope| scope.id == self.current_scope_id)
            .last()
        {
            scope
        } else {
            // panic with the full string to make it easier to find the issue
            let scopes_str = self
                .scopes
                .iter()
                .map(|scope| format!("{}", scope))
                .collect::<Vec<String>>()
                .join("\n");
            panic!(
                "no current scope with id: {}. Scopes: {:?}",
                self.current_scope_id, scopes_str
            );
        }
    }

    pub fn parent_scope(&self) -> Option<&SymbolTableScope> {
        if let Some(scope) = self
            .scopes
            .iter()
            .filter(|scope| scope.id == self.current_scope().parent.unwrap_or(0))
            .last()
        {
            Some(scope)
        } else {
            None
        }
    }

    pub fn current_scope_mut(&mut self) -> &mut SymbolTableScope {
        self.scopes
            .iter_mut()
            .filter(|scope| scope.id == self.current_scope_id)
            .last()
            .expect("no current scope")
    }

    pub fn current_scope_type(&self) -> &SymbolTableType {
        return &self.current_scope().kind;
    }

    /// Returns scopes until the given position
    /// the scopes are sorted by start position descending
    pub fn innermost_scope(&self, pos: usize) -> &SymbolTableScope {
        log::debug!("looking for innermost scope");
        let scope = self.current_scope();
        if pos == 90 {
            log::debug!("scopes: {:#?}", scope);
        }

        return scope;
    }

    /// get innermost scope that contains that line
    /// search for symbol in that scope
    /// if not found search in parent scope
    /// continue until found or no parent scope
    /// TODO: This function does not work on the literal test
    pub fn lookup_in_scope(&self, lookup_request: LookupSymbolRequest) -> Option<&SymbolTableNode> {
        let mut innermost_scope = Some(self.innermost_scope(0));
        while let Some(scope) = innermost_scope {
            log::debug!("looking in scope: {:?}", scope.name);
            if let Some(symbol) = scope.symbols.get(&lookup_request.name) {
                return Some(symbol);
            }
            // We reach the global scope
            if scope.parent.is_none() {
                break;
            }
            innermost_scope = if let Some(parent_id) = scope.parent {
                self.scopes
                    .iter()
                    .filter(|scope| scope.id == parent_id)
                    .last()
            } else {
                Some(self.global_scope())
            }
        }
        None
    }

    /// Creates a new scope and sets it as the current scope
    pub fn push_scope(&mut self, new_scope: SymbolTableScope) {
        self.current_scope_id = new_scope.id;
        self.scopes.push(new_scope);
    }

    /// Sets the current scope to the scope that starts at the given position
    pub fn set_scope(&mut self, pos: usize) {
        self.prev_scope = Some(self.current_scope_id);
        let scope = self.scopes.iter().find(|scope| scope.start_pos == pos);
        if let Some(scope) = scope {
            self.current_scope_id = scope.id;
        } else {
            panic!("no scope found for position: {}", pos);
        }
    }

    pub fn revert_scope(&mut self) {
        self.current_scope_id = self.prev_scope.expect("no previous scope");
    }

    pub fn global_scope(&self) -> &SymbolTableScope {
        self.scopes
            .iter()
            .filter(|scope| scope.id == 0)
            .last()
            .expect("no global scope")
    }

    pub fn exit_scope(&mut self) {
        self.current_scope_id = self
            .current_scope()
            .parent
            .expect("no parent scope. Exiting global scope is not allowed");
    }

    pub fn add_symbol(&mut self, mut symbol_node: SymbolTableNode) {
        let scope = self.current_scope_mut();
        if let Some(existing_symbol) = scope.symbols.get(&symbol_node.name) {
            symbol_node
                .declarations
                .extend(existing_symbol.declarations.clone());
        } else {
            scope.symbols.insert(symbol_node.name.clone(), symbol_node);
        }
    }

    // TODO: this can be attribute of symbol table
    pub fn is_pyi(&self) -> bool {
        self.file_path.extension().unwrap() == "pyi"
    }

    /// Looks up an attribute in the current scope and its parents
    /// Attributes must have symbol flags CLASS_MEMBER or INSTANCE_MEMBER
    pub(crate) fn lookup_attribute(&self, attr: String) -> Option<&SymbolTableNode> {
        let mut innermost_scope = Some(self.innermost_scope(0));
        while let Some(scope) = innermost_scope {
            log::debug!("looking for attribute in scope: {:?}", scope.name);
            if let Some(symbol) = scope.symbols.get(&attr) {
                if symbol.flags.contains(SymbolFlags::CLASS_MEMBER)
                    || symbol.flags.contains(SymbolFlags::INSTANCE_MEMBER)
                {
                    return Some(symbol);
                }
            }
            // We reach the global scope
            if scope.parent.is_none() {
                break;
            }
            innermost_scope = if let Some(parent_id) = scope.parent {
                self.scopes
                    .iter()
                    .filter(|scope| scope.id == parent_id)
                    .last()
            } else {
                Some(self.global_scope())
            }
        }
        None
    }
}

impl SymbolTableNode {
    pub fn add_declaration(&mut self, decl: Declaration) {
        self.declarations.push(decl);
    }

    pub fn last_declaration(&self) -> &Declaration {
        self.declarations
            .last()
            .expect("There must be at least one declaration")
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
        let mut sorted_scopes = self.scopes.iter().collect::<Vec<&SymbolTableScope>>();
        sorted_scopes.sort_by(|a, b| a.name.cmp(&b.name));

        for scope in sorted_scopes.iter() {
            // Skip printing the builtin scope
            if matches!(scope.kind, SymbolTableType::BUILTIN) {
                continue;
            }
            writeln!(f, "{}", scope)?;
        }

        writeln!(f, "Scopes:\n")?;
        for scope in sorted_scopes.iter() {
            writeln!(
                f,
                "Scope {}(id: {:?}, parent_id: {:?}",
                scope.name, scope.id, scope.parent
            )?;
        }

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

        writeln!(
            f,
            "Symbols in {} (id: {}, parent: {:?})",
            self.name, self.id, self.parent
        )?;
        for (name, symbol) in sorted_symbols {
            writeln!(f, "{}", name)?;
            writeln!(f, "{:#?}", symbol.flags)?;
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
            Declaration::TypeParameter(t) => write!(f, "{:#?}", t),
            Declaration::TypeAlias(t) => write!(f, "{:#?}", t),
        }
    }
}
