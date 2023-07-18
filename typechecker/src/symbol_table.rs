use parser::ast::Node;
use std::collections::HashMap;

#[derive(Debug)]
pub struct SymbolTable {
    pub symbol_table_type: SymbolTableType,
    symbols: HashMap<String, SymbolTableNode>,
    pub start_line_number: u8,
    // all sub tables have to be valid until the top level scope is valid
    // sub_tables: Vec<&'a SymbolTable<'a>>,
    // index of current scope in this table where we insert new symbols
    // current_scope: u8,
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
    pub node: Node,
    pub typ: NodeType,
    pub module_public: bool,
    pub module_hidden: bool,
    pub implicit: bool,
    pub scope: SymbolScope,
}

#[derive(Debug)]
pub enum NodeType {
    String,
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
        SymbolTable {
            symbol_table_type,
            symbols: HashMap::new(),
            start_line_number,
        }
    }
    // pub fn lookup_in_scope(&self, name: &str) -> Option<&SymbolTableNode> {
    //     return self.symbols.get(name);
    // }
    //
    // pub fn enter_scope(&mut self, new_symbol_table: &'a SymbolTable<'a>) {
    //     self.sub_tables.push(new_symbol_table);
    // }
    pub fn exit_scope(&self) {}

    pub fn add_symbol(&mut self, symbol_node: SymbolTableNode) {
        self.symbols.insert(symbol_node.name.clone(), symbol_node);
    }
}
