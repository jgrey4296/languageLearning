from random import randrange
import IPython

#Orthagonal neighbours, not diagnonal
neighbours = [
    [-1, 0], [1, 0], #X coord neighbours
    [0, -1], [0, 1]  #Y coord neighbours
]

class Grid:
    """ A 2D Grid Generator """

    def __init__(self, size, maxcost=10):
        self.grid = []
        for y in range(size):
            self.grid.append([])
            for x in range(size):
                self.grid[y].append(randrange(0, maxcost))

    def __repr__(self):
        outstr = ""
        xSize, ySize = self.size()
        for i, line in enumerate(self.grid):
            for j, pos in enumerate(line):
                if (i, j) == (0, 0) or (i, j) == (xSize - 1, ySize - 1):
                    outstr += "^{}^ ".format(str(pos))
                else:
                    outstr += "|{}| ".format(str(pos))
            outstr += "\n"
        return outstr

    def repr_path(self, path):
        outstr = ""
        for i, line in enumerate(self.grid):
            for j, pos in enumerate(line):
                if (i, j) in path:
                    outstr += "^{}^ ".format(str(pos))
                else:
                    outstr += "|{}| ".format(str(pos))
            outstr += "\n"
        return outstr

    def size(self):
        return (len(self.grid), len(self.grid[0]))


    def __getitem__(self, n):
        return self.grid[n]

    def get(self, locTuple):
        assert(isinstance(locTuple, tuple))
        return self.grid[locTuple[0]][locTuple[1]]

    def neighbours(self, locTuple):
        assert(isinstance(locTuple, tuple))
        x,y = locTuple
        xSize, ySize = self.size()
        local_neighbours = []
        for xMod, yMod in neighbours:
            xPrime = x + xMod
            yPrime = y + yMod
            if 0 <= xPrime < xSize and \
               0 <= yPrime < ySize:
                local_neighbours.append((xPrime, yPrime))
        return local_neighbours
