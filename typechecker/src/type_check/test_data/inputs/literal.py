from typing import Literal


a: Literal["foo"] = "foo"


class Foo:
    def __init__(self, name: Literal["foo"]) -> None:
        self.name = name


def func(literal_name: Literal["foo"]) -> None:
    print(literal_name)
