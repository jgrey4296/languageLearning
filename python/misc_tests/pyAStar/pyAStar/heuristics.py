from math import sqrt
#--------------------
#Heuristics
#--------------------
def distance(start, target):
    x = pow(target[0] - start[0], 2)
    y = pow(target[1] - start[1], 2)
    return sqrt(x + y)

#--------------------
heuristics = {
    "dist" : distance
    }
