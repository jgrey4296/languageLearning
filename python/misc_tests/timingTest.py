import timeit

def test():
    return list([x for x in range(1000)])

def compTest1():
    return [1 != 10 for x in range(1000)]

def compTest2():
    return ["#" in "#a" for x in range(1000)]

def compTest3():
    return ["#" in "aawegaw#" for x in range(1000)]

def compTest4():
    return ["#" in "awggewehrh" for x in range(1000)]

def compTest5():
    return [2 & 12523 for x in range(1000)]


print("Test 1: " + str(timeit.timeit(compTest1)))

print("Test 2: " + str(timeit.timeit(compTest2)))
print("Test 3: " + str(timeit.timeit(compTest3)))
print("Test 4: " + str(timeit.timeit(compTest4)))
print("Test 5: " + str(timeit.timeit(compTest5)))




