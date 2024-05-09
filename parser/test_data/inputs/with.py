with a: pass

with a as b: pass

with a as b, c as d: pass

with (a as b, c as d): pass

async with a as b: pass

async with a as b, c as d:
    pass

async with (
        a as b, c as d
):
    a = 1
