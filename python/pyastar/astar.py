from collections import defaultdict
from math import inf, sqrt
import heapq
import logging as root_logger

from .heuristics import heuristics

logging = root_logger.getLogger(__name__)
InfCosts = lambda: defaultdict(lambda: inf)

class AStar:
    """ A Generalised A Star method class,
    implemented from the pseudocode on wiki """

    def __init__(self, heuristic="dist"):
        if heuristic in heuristics:
            self.heuristic = heuristics[heuristic]
        elif callable(heuristic):
            self.heuristic = heuristic
        else:
            raise Exception("Heuristic not recognised")


    def __call__(self, grid, startTuple, targetTuple):
        """ Run the calc on the provided grid """
        logging.debug("Starting Pathfinding from {} to {}".format(str(startTuple), str(targetTuple)))
        #Keys / sets use tuples for locations
        discovered = set()

        #frontier :: Heap [HeapNode]
        frontier = [HeapNode(startTuple, grid.get(startTuple))]
        #Origins : { (x,y): (x,y) }
        origins = {}
        #Node_Costs : { (x,y) : number }
        node_costs = InfCosts()
        #initialise start cost:
        node_costs[startTuple] = 0

        while bool(frontier):
            current = heapq.heappop(frontier)
            logging.debug("Current: {}".format(repr(current)))
            if current.loc == targetTuple:
                #Arrived, reconstruct the path
                logging.debug("Reached end, returning path")
                return self.reconstruct_path(targetTuple, origins)
            discovered.add(current.loc)
            neighbours = grid.neighbours(current.loc)
            logging.debug("Found neighbours: {}".format(" ".join([str(x) for x in neighbours])))
            for neighbour in neighbours:
                if neighbour in discovered:
                    continue
                neighbour_cost = grid.get(neighbour)
                node_cost = node_costs[current.loc] + neighbour_cost
                target_cost = node_cost + self.heuristic(neighbour, targetTuple)
                if neighbour not in frontier:
                    logging.debug("Adding to frontier: {}".format(str(neighbour)))
                    heapq.heappush(frontier, HeapNode(neighbour, target_cost))

                if node_cost >= node_costs[neighbour]:
                    continue

                origins[neighbour] = current.loc
                node_costs[neighbour] = node_cost


        return []


    def reconstruct_path(self, targetTuple, origins):
        current = targetTuple
        path = [current]
        while current in origins:
            current = origins[current]
            path.append(current)
        path.reverse()
        return path


class HeapNode:
    """ A Simple Orderable class for use in the heap """
    def __init__(self, loc, amnt):
        assert(isinstance(loc, tuple))
        self.loc = loc
        self.amnt = amnt

    def __lt__(self, other):
        assert(isinstance(other, HeapNode))
        return self.amnt < other.amnt

    def __eq__(self, other):
        if isinstance(other, HeapNode):
            return self.loc == other.loc
        else:
            return self.loc == other

    def __repr__(self):
        return str(self.loc)
