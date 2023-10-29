class a: pass

class a():
    pass

class a(b, c): pass

class a(b, *c, **d): pass

class a(b,
c,
d): pass

@decor
class a: pass

class a[T]: pass

class a[T, U]: pass

class a[T: U]: pass

class a[T: U, V: W]: pass

class a[*T]: pass

class a[T, *U]: pass

class a[T: U, *V]: pass

class a[T: U, V: W, *X]: pass

class a[**T]: pass

class a[T, **U]: pass

class a[T: U, **V]: pass

class a[T: U, V: W, **X]: pass

class a[T, *U, **V]: pass

class a[T: U, *V, **W]: pass
