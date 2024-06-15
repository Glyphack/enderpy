(a for a in b)

(a for a in b if c)

(a for a in b if c if d)

(a for a in b for c in d)

(ord(c) for line in file for c in line)

(x*y for x in range(10) for y in range(10) if x*y > 10)

has_explicit_annotation = isinstance(fdef.type, CallableType) and any(
    not is_unannotated_any(t) for t in fdef.type.arg_types + [fdef.type.ret_type]
)
