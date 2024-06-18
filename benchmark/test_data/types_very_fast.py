from typing import Dict, Set, List, Tuple, Any
import math
import os
import sys
import json
import time
import datetime
import random
import statistics
import functools
import itertools

a = 1
b = "1"
c = True
d = False
_ = a + int(b)
_ = a + c


def func(params: List[int]) -> Dict[int, int]:
    return {params[0]: a}


class C:
    cls_attribute = {1}

    def __init__(self, x: int):
        print(self.cls_attribute)
        self.x = float(x)
        print(self.x)

    def add(self, value: int) -> None:
        self.cls_attribute.add(value)

    def get_attr(self) -> Set[int]:
        return self.cls_attribute

    def get_x(self) -> float:
        return self.x


t = C(0)
t.add(2)
t.cls_attribute
t.x
t.get_x()
t.get_attr()

l = [1, 2, 3]
d = {"a": 1, "b": 2}
s = {1, 2, 3}

l.append(4)
