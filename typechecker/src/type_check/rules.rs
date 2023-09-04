use super::types::Type;

pub fn is_reassignment_valid(old_type: &Type, new_type: &Type) -> bool {
    if old_type == &Type::Unknown {
        return true;
    }
    if old_type == new_type {
        return true;
    }

    return false;
}
