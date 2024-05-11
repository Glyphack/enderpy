# Test case to check that we return correct error when two python statements are on the same line

def foo():
    x = 1; y = 2
    return x + y

def bar():
    # TODO(parser): enable after error handling
    # x = 1 y = 2
    return x + y

# TODO(parser): enable after error handling
# a = 1  b = 2
