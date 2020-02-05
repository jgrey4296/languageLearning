from payofftree import Tree as pTree
from operator import itemgetter
from random import choice

import IPython

class Game(object):
    """ The main PrisonersDilemma Game """

    #todo: add a null action for games where some players dont always play

    def __init__(self):
        self.players = {}   #The players of the game who make decisions
        self.pseudo_players = {} #random processes that happen
        self.actions = {}        #performances that change information or player state
        self.information = []

        self.play_order = []     #The order (pseudo-)players are sorted in payoff tree

        """ Payoffs: a tree of depth len(players) + len(pseudo_players)
            whose leaves provide the payoff values for that profile of actions
            Nodes are actions, players are implicit in the ordering
        """
        self.payoffs = pTree()

    def setPlayerOrder(self,*players):
        """ Set the order of turns for players, and the depth of the payoff tree
            they inhabit """
        for player in players:
            if player not in self.players.values() and player not in self.pseudo_players.values():
                raise Exception("Unrecognised Player: {}".format(player.name))
        self.play_order = players

    def addPlayer(self,*playerNames):
        """ Add new player objects to the game, named by the list passed in """
        for name in playerNames:
            newPlayer = Player(name)
            self.players[name] = newPlayer
        return list(self.players.values())

    def addPseudoPlayer(self,*playerNames):
        for name in playerNames:
            newPseudo = Player(name,pseudo=True)
            self.pseudo_players[name] = newPseudo
        return list(self.pseudo_players.values())


    def addAction(self,player,actionName):
        """ Add a new action to a players action set """
        if player.name not in self.players.keys() and player.name not in self.pseudo_players.keys():
            raise Exception("Player not found: {}".format(player.name))
        newAction = Action(actionName)
        player.action_set.append(newAction)
        self.actions[newAction.name] = newAction
        return newAction

    def addPayoff(self,*triples):
        """ Associate a payoff utility with an action profile """
        self.payoffs.clearCache()
        depthTriples = [(self.play_order.index(actor)+1,actor,action,payoff) for (actor,action,payoff) in triples]
        sortedDepthTriples = sorted(depthTriples,key=itemgetter(0))
        payoffLeafValues = []
        for (i,actor,action,payoff) in sortedDepthTriples:
            if action not in actor.action_set:
                raise Exception("Action not found for actor: {},{}".format(action.name,actor.name))
            #Add the actors payoff to the leaf record:
            payoffLeafValues.append((actor,payoff))
            #Add a child to the cached position in the payoffs tree
            self.payoffs.insert_child(i,action)

        #add the payoff leaf to the last child:
        self.payoffs.storeLeafValues(payoffLeafValues)
        self.payoffs.clearCache()

    def setProbability(self,*actionProbPairs):
        """ Set the probability distribution for a set of actions """
        probTotal = sum([prob for (action,prob) in actionProbPairs])
        if probTotal != 1:
            raise Exception("Specified probabilities do not sum to 1")
        for action,prob in actionProbPairs:
            action.probability = prob


    def setConditionalProbability(self,condition,action,prob,negativeAction):
        """ Set the probability of an action being taken as part of an action profile"""
        #todo
        return


    def play(self,*moves,reset=True):
        if reset:
            self.reset()
        if len(moves) == 0:
            #randomly choose moves
            sortedDepthPairs = [(i,player,choice(player.action_set)) for (i,player) in enumerate(self.play_order)]
            #--
        else:
            depthPairs = [(self.play_order.index(actor),actor,action) for (actor,action) in moves]
            sortedDepthPairs = sorted(depthPairs,key=itemgetter(0))

        sortedActions = [action for (i,actor,action) in sortedDepthPairs]
        #navigate down the payoff tree move by move
        playerPayoffPair = self.payoffs.followActionPath(sortedActions)
        #apply the payoffs
        for (actor,delta) in playerPayoffPair:
            actor.cumulative_utility += delta
        return [delta for (actor,delta) in playerPayoffPair if not actor.isPseudo]

    def playIterative(self,*moves,n=1):
        self.reset()
        sumDeltas = None
        for move in range(n):
            deltas = self.play(reset=False)
            if sumDeltas is None:
                sumDeltas = deltas
            else:
                sumDeltas = [x+y for x,y in zip(sumDeltas,deltas)]
        return sumDeltas

    def reset(self):
        for actor in self.players.values():
            actor.reset()

    def monte_carlo_optimum(self,n=10):
        #for n full games, play random games and track the
        #likelihood and payoff of each path
        #return the optimum
        return


#--------------------
#Components of the Game:
#--------------------
class Player(object):

    def __init__(self,name,pseudo=False):
        self.name = name
        self.cumulative_utility = 0
        self.current_state = []
        self.action_set = []
        self.isPseudo = pseudo

    def reset(self):
        self.cumulative_utility = 0
        self.current_state = []

    def __str__(self):
        return self.name

class Action(object):

    def __init__(self,name):
        self.name = name
        self.players = []
        self.probability = 1


    def __str__(self):
        return self.name
