from typing import Union, Literal
from enum import Enum

class Empty(Enum):
    token = 0

a: Union[str, int]

b: str | None

c: Union[str, int, None]

# failing test enable after handling Union types with more than 2 types
c: str | Empty | None

c: str | Empty | None | int
