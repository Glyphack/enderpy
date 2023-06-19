use parser::ast;
use std::collections::HashMap;

pub struct SymbolTable<'a> {
    pub name: String,
    pub symbol_table_type: SymbolTableType,
    pub symbols: HashMap<String, SymbolTableNode>,
    pub start_line_number: u8,
    // all sub tables have to be valid until the top level scope is valid
    pub sub_tables: Vec<&'a SymbolTable<'a>>,
    pub current_scope: u8,
}

pub enum SymbolTableType {
    Module,
    Class,
    Function,
}

pub struct SymbolTableNode {
    pub name: String,
    pub node: ast::Statement,
    pub typ: NodeType,
    pub module_public: bool,
    pub module_hidden: bool,
    pub implicit: bool,
    pub scope: SymbolScope,
}

pub enum NodeType {
    String,
}

pub enum SymbolScope {
    Global,
    Nonlocal,
    Local,
    Unknown,
}

impl<'a> SymbolTable<'a> {
    pub fn lookup_in_scope(&self, name: &str) -> Option<&SymbolTableNode> {
        return self.symbols.get(name);
    }

    pub fn enter_scope(&mut self, new_symbol_table: &'a &SymbolTable<'a>) {
        self.sub_tables.push(new_symbol_table);
    }
    pub fn exit_scope(&self) {}
}
