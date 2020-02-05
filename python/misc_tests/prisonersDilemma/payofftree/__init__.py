""" A Tree structure to create the decision tree for payoffs for a formalised game """
import IPython

class Tree(object):

    def __init__(self):
        self.root = Node('root')
        self.cacheNode = self.root
        self.cacheDepth = 0

    def clearCache(self):
        self.cacheNode = self.root
        self.cacheDepth = 0

    def insert_child(self,depth,action):
        if self.cacheDepth+1 != depth:
            raise Exception("Cache Depth Mismatch: {}/{}".format(self.cacheDepth,depth))
        potentialNode = self.cacheNode.getChildForAction(action)
        if potentialNode is None:
            potentialNode = Node(action)
            self.cacheNode.children.append(potentialNode)
        self.cacheDepth += 1
        self.cacheNode = potentialNode

    def storeLeafValues(self,values):
        if len(self.cacheNode.children) > 0:
            raise Exception("Trying to store payoff values in non-leaf")
        self.cacheNode.payoffs = values

    def followActionPath(self,actionList):
        currentNode = self.root
        for action in actionList:
            currentNode = currentNode.getChildForAction(action)
        return currentNode.payoffs


class Node(object):

    def __init__(self,action):
        self.action = action
        self.children = []
        #invariant 1: len(children) == len(probabilities)
        #invariant 2: sum(probabilities) == 1
        self.probabilities = []
        self.payoffs = None

    def getChildForAction(self,action):
        for child in self.children:
            if child.action == action:
                return child
        return None
