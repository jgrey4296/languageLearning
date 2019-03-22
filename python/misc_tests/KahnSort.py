import networkx as nx
from collections import defaultdict
import logging as root_logger
import IPython
logging = root_logger.getLogger(__name__)

inf = [float("inf")]

class KahnSort:

    @staticmethod
    def sort(graph, initial):
        """ Returns Either (Layers, Conflict) """
        assert(isinstance(graph, nx.DiGraph))
        assert(isinstance(initial, set))
        #initialise
        theGraph = nx.DiGraph(graph)
        exhausted = set()
        frontier = set(initial)
        maxLayer_map = defaultdict(lambda: 0)
        #explore the frontier
        while bool(frontier):
            current = frontier.pop()
            exhausted.add(current)
            #update the maxLayer count
            maxLayer = max([0] + [maxLayer_map[x] for x in KahnSort.input_edges(graph, current)])
            maxLayer_map[current] = maxLayer + 1
            #follow the edges
            for x in nx.DiGraph(theGraph[current]):
                maxLayer_map[x] = max(maxLayer_map[current], maxLayer_map[x])
                theGraph.remove_edge(current, x)
                if not bool(KahnSort.input_edges(theGraph, x)):
                    frontier.add(x)

        if bool(theGraph.edges()):
            #detect a conflict
            active = set(maxLayer_map.keys()).difference(exhausted)
            activePairs = [(x,maxLayer_map[x]) for x in active]
            #get the smallest active
            smallestActive = min([(None,float("inf"))] + activePairs, key=lambda x: x[1])[0]
            #get its input edges
            smallestInputs = KahnSort.input_edges(theGraph, smallestActive)
            #get the largest input edge
            ancestors = [(x,maxLayer_map[x]) for x in smallestInputs]
            maxAncestor = max([(None, -1)] + ancestors, key=lambda x: x[1])[0]
            IPython.embed(simple_prompt=True)
            return (None, (maxAncestor, smallestActive))
        else:
            return (maxLayer_map, None)

    @staticmethod
    def input_edges(graph, node):
        """ Test whether a node has input edges """
        edges = [a for (a,b) in graph.edges() if b == node]
        return edges
