match a:
    case 1:
        pass

match a:
    case 1 | 2:
        pass

match a.b:
    case 1:
        pass

match a:
    case None:
        pass
    case True:
        pass
    case False:
        pass
    case -1:
        pass
    case 1.0:
        pass
    case _:
        pass

match a:
    case a.b:
        pass
    case a:
        pass

match a:
    case (a, b):
        pass
    case {1: _ ,2: _}:
        pass
    case {**rest}:
        pass

match x:
    case Point2D(0, 0):
        pass
    case Point3D(x=0, y=0, z=0):
        pass

match x:
    case [a, b, c]:
        pass
