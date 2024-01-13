from abc import abstractmethod


def f():
    raise Exception('This is the error message.')

a = f()

class A:
    @abstractmethod
    def f2():
        raise Exception('This is the error message.')

    b = f2()

