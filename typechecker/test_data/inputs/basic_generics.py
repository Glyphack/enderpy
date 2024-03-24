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
