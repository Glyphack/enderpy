import symtable

def print_symbol_table_info(symbol_table: symtable.SymbolTable, indent=0):
    # Print the information of the current symbol table
    print(" " * indent + f"Symbol Table: {symbol_table.get_name()}")
    print(" " * (indent + 2) + f"Type: {symbol_table.get_type()}")
    print(" " * (indent + 2) + f"Identifier: {symbol_table.get_id()}")
    print(" " * (indent + 2) + f"First Line Number: {symbol_table.get_lineno()}")

    # Print information specific to Function symbol table
    if symbol_table.get_type() == 'function':
        function_info = symbol_table.get_parameters(), symbol_table.get_locals(), \
                        symbol_table.get_globals(), symbol_table.get_nonlocals(), \
                        symbol_table.get_frees()
        print(" " * (indent + 2) + f"Function Info: {function_info}")

    # Print information specific to Class symbol table
    if symbol_table.get_type() == 'class':
        class_methods = symbol_table.get_methods()
        print(" " * (indent + 2) + f"Class Methods: {class_methods}")

    # Print information about each symbol in the symbol table
    for symbol in symbol_table.get_symbols():
        print(" " * (indent + 2) + f"Symbol: {symbol.get_name()}")
        print(" " * (indent + 4) + f"Referenced: {symbol.is_referenced()}")
        print(" " * (indent + 4) + f"Imported: {symbol.is_imported()}")
        print(" " * (indent + 4) + f"Parameter: {symbol.is_parameter()}")
        print(" " * (indent + 4) + f"Global: {symbol.is_global()}")
        print(" " * (indent + 4) + f"Nonlocal: {symbol.is_nonlocal()}")
        print(" " * (indent + 4) + f"Declared Global: {symbol.is_declared_global()}")
        print(" " * (indent + 4) + f"Local: {symbol.is_local()}")
        print(" " * (indent + 4) + f"Annotated: {symbol.is_annotated()}")
        print(" " * (indent + 4) + f"Free: {symbol.is_free()}")
        print(" " * (indent + 4) + f"Assigned: {symbol.is_assigned()}")
        print(" " * (indent + 4) + f"Namespace: {symbol.is_namespace()}")
        print(" " * (indent + 4) + f"Namespaces: {symbol.get_namespaces()}")
        # print namespaces (redundant because repeated in children)
        # for namespace in symbol.get_namespaces():
        #     print(" " * (indent + 6) + f"Namespace: {namespace}")
       #     print_symbol_table_info(namespace, indent + 8)

    # Recursively print information of child symbol tables
    for child_table in symbol_table.get_children():
        print_symbol_table_info(child_table, indent + 2)

# Example usage:
code = """
a = 1
"""
symbol_table = symtable.symtable(code, "example", "exec")
print_symbol_table_info(symbol_table)

