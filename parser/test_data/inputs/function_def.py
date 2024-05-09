def a(): pass

def a():
    pass

def a(a, b, c): pass

def a(a, *b, **c): pass

def a(a,
b,
c): pass

@decor
def a(): pass

@decor
def f(a: 'annotation', b=1, c=2, *d, e, f=3, **g): pass

def func() -> None: pass

async def a(): pass

async def a():
    pass

async def a(a, b, c): pass

def a[T](): pass

def a[T, U](): pass

def a[T: U](): pass

def a[T: U, V: W](): pass

def a[*T](): pass

def a[T, *U](): pass

def a[T: U, *V](): pass

def a[T: U, V: W, *X](): pass

def a[**T](): pass

def a[T, **U](): pass

def a[T: U, **V](): pass

def a[T: U, V: W, **X](): pass

def a[T, *U, **V](): pass

def a[T: U, *V, **W](): pass

def dataclass_transform(
    *,
    eq_default: bool = True,
    order_default: bool = False,
    kw_only_default: bool = False,
    frozen_default: bool = False,  # on 3.11, runtime accepts it as part of kwargs
    # this next annotation cannot be parsed need fix
    field_specifiers: tuple[type[Any] | Callable[..., Any], ...] = (),
    **kwargs: Any,
) -> IdentityFunction: ...
