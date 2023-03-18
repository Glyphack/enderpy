pub fn extract_string_inside(val: String) -> String {
    if let Some(val) = val.strip_prefix("\"\"\"") {
        val.strip_suffix("\"\"\"")
            .expect("String must be enclosed with \"\"\"")
            .to_string()
    } else if let Some(val) = val.strip_prefix("\"") {
        val.strip_suffix("\"")
            .expect("String must be enclosed with \"")
            .to_string()
    } else if let Some(val) = val.strip_prefix("'''") {
        val.strip_suffix("'''")
            .expect("String must be enclosed with '''")
            .to_string()
    } else if let Some(val) = val.strip_prefix("'") {
        val.strip_suffix("'")
            .expect("String must be enclosed with '")
            .to_string()
    } else {
        panic!("String must be enclosed in \"\"\", \"', ''' or '");
    }
}
