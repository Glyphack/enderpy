from logging import Logger
from typing import Generic, Sequence, TypeVar, Iterable

T = TypeVar("T")


def first(l: Sequence[T]) -> T:
    return l[0]


first([1, 2, 3])  # return type type parameter


AnyStr = TypeVar("AnyStr", str, bytes)


def concat(x: AnyStr, y: AnyStr) -> AnyStr:
    return x + y


# > Specifying a single constraint is disallowed.

BadConstraint1 = TypeVar("BadConstraint1", str)  # Type error

# > Note: those types cannot be parameterized by type variables

BadConstraint2 = TypeVar("BadConstraint2", str, T)  # Type error


class LoggedVar(Generic[T]):
    def __init__(self, value: T, name: str, logger: Logger) -> None:
        self.name = name
        self.logger = logger
        self.value = value

    def set(self, new: T) -> None:
        self.log("Set " + repr(self.value))
        self.value = new

    def get(self) -> T:
        self.log("Get " + repr(self.value))
        return self.value

    def log(self, message: str) -> None:
        msg = "{}: {}".format(self.name, message)
        self.logger.info(msg)


log_var = LoggedVar(1, "var1", Logger("test"))


def zero_all_vars(vars: Iterable[LoggedVar[int]]) -> None:
    for var in vars:
        var.set(0)


zero_all_vars([log_var])  # OK


# > A generic type can have any number of type variables, and type variables
# > may be constrained.

S = TypeVar("S")


class Pair1(Generic[T, S]): ...


pair1 = Pair1()

# > Each type variable argument to ``Generic`` must be distinct.


class Pair2(Generic[T, T]):  # Type error
    ...


pair2 = Pair2()

# > The ``Generic[T]`` base class is redundant in simple cases where you
# > subclass some other generic class and specify type variables for its
# > parameters.

from collections.abc import Iterator, Mapping


class MyIter1(Iterator[T]): ...


class MyIter2(Iterator[T], Generic[T]): ...


iter1 = MyIter1()
iter2 = MyIter2()

K = TypeVar("K")
V = TypeVar("V")


class MyMap1(Mapping[K, V], Generic[K, V]): ...


my_map1 = MyMap1()


class MyMap2(Mapping[K, V], Generic[V, K]): ...


my_map2 = MyMap2()


def test_my_map(m1: MyMap1[str, int], m2: MyMap2[int, str]):
    a = m1["key"]  # int
    b = m2["key"]  # int

    m1[0]  # Type error
    m2[0]  # Type error


# > You can use multiple inheritance with ``Generic``

from collections.abc import Sized, Container


class LinkedList(Sized, Generic[T]): ...


class MyMapping(Iterable[tuple[K, V]], Container[tuple[K, V]], Generic[K, V]): ...


my_mapping = MyMapping()


# > Subclassing a generic class without specifying type parameters assumes
# > ``Any`` for each position.  In the following example, ``MyIterable``
# > is not generic but implicitly inherits from ``Iterable[Any]``::


class MyIterableAny(Iterable):  # Same as Iterable[Any]
    ...


def test_my_iterable_any(m: MyIterableAny):
    iter(m)  # Iterator[Any]


# > Generic metaclasses are not supported


class GenericMeta(type, Generic[T]): ...


generic_meta = GenericMeta()


class GenericMetaInstance(metaclass=GenericMeta[T]):  # Type error
    ...


generic_meta_instance = GenericMetaInstance()
