use super::types::PythonType;

pub fn is_reassignment_valid(old_type: &PythonType, new_type: &PythonType) -> bool {
    if old_type == &PythonType::Unknown {
        return true;
    }
    if old_type == new_type {
        return true;
    }

    false
}
