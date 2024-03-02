a = 1
b = "1"
c = True
d = False
a + int(b)


a + c

def func(param1: int) -> int:
	return param1 + a

class C:
	cls_attribute = 1

	def __init__(self, x: int):
		print(self.cls_attribute)
		self.x = float(x)
		print(self.x)

	def add(self, value: int):
		self.cls_attribute += value

	def get_attr(self) -> int:
		return self.cls_attribute

	def	get_x(self) -> float:
		return self.x

t = C(0)
t.add(2)
t.cls_attribute
t.x
t.get_x()
t.get_attr()

l = [1,2,3]
d = {"a": 1, "b": 2}
s = {1,2,3}

l.append(4)

