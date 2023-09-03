a: list[int] = [1, 2, 3]

b = a[0] + 1

c = a[0] + a[1]

# invalid usage of types
d = a[0] + "str"


# valid reassignment
a = [1]
# invalid reassignment
a = [1, 2, "str"]
