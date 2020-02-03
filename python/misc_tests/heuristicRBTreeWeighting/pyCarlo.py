"""
An attempt at Monte Carlo Tree Search in Python
Modelling conversation

#todos:
Participants ($p1, $p2...) to fill in at runtime
Have Turn markers. $end, and $interrupt, $continue
Add conditions for run before selecting in get_next_move_based_on
(conditions don't fact in calculation of tree)
Guarded cycles

######
guarded cycles: stateful loops that carry the paths taken previously, to protect
against infinite loops

"""
# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log_pycarlo.log"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################
from collections import namedtuple, defaultdict
from uuid import uuid1
from random import choice
import pickle

#The leaf of a simulated descent, with a list of effects to run
LeafSim = namedtuple('LeafSim', 'node effects')

#------------------------------------------------------------
class MCTSD_Tree:
    """ An authored Tree for Dialogue that simulates using MTCS """

    def __init__(self, min_children=3):
        self.min_children = min_children
        self.root_node = Node("Hello", [])

        #dict<uuid, node>
        self.nodes = {self.root_node.uuid : self.root_node}

    def construct(self):
        """ Get a node that has under the minimum children, and ask for an expansion """
        #Get all the nodes that DONT have the value 'end':
        relevant_nodes = [x for x in self.nodes.values() if x.text != "End"]
        #Then filter for number of children:
        nodes_needing_children = [x for x in relevant_nodes if len(x) < self.min_children]
        while len(nodes_needing_children) > 0:
            #select a node:
            selected = choice(nodes_needing_children)
            if selected.parent is not None:
                parent_text = self.nodes[selected.parent].text
            else:
                parent_text = "Start: "
            #Ask for a child
            print('\n----------')
            print("{}".format(parent_text))
            print("{}".format(selected.text))
            next_node_text = input("Response: ")
            if next_node_text == "$exit":
                break
            print(next_node_text)
            effects = input("Effects: ")
            #todo: parse effects
            split_effects = effects.split(',')
            #todo: annotate effects
            new_node = Node(next_node_text, split_effects, parent=selected)
            self.nodes[new_node.uuid] = new_node

            #refilter nodes and repeat
            relevant_nodes = [x for x in self.nodes.values() if x.text != "End"]
            #Then filter for number of children:
            nodes_needing_children = [x for x in relevant_nodes if len(x) < self.min_children]
            #repeat
        print('Finished')

    def get_next_move_based_on(self, current, weighting):
        """ Given a personal utility set, choose the best move """
        assert isinstance(current, Node)
        assert isinstance(weighting, list)
        None


    def calculate_tree(self, iterations=100):
        """ The Monte-Carlo Simulation of the tree """
        for i in range(iterations):
            focus = choice(self.nodes.values())
            leaf = self._expand(focus)
            result = self._evaluate(leaf)
            self._backprop(leaf, result)


    def _evaluate(self, leaf):
        """ Given an amassed sequence of effects, log the consequences to add to each node """
        assert isinstance(leaf, LeafSim)
        # [effects] -> {effectType: delta}
        return Result()

    def _expand(self, node, target_depth=500):
        """ Given a node, expand until you hit a leaf,
        storing the effects of the choices as you go
        """
        assert isinstance(node, Node)
        expansions = 0
        effects = []
        current = node
        while len(current) > 0 and expansions < target_depth:
            effects.append(current.effects)
            current = self.nodes[current.a_child()]
            expansions += 1
        return LeafSim(current, effects)

    def _backprop(self, leaf, result):
        assert isinstance(leaf, LeafSim)
        assert isinstance(result, Result)
        current = leaf.node
        while current is not None:
            current.update(result)
            current = self.nodes[current.parent]

#------------------------------------------------------------
class Node:
    """ A node in a conversation tree that contains:
    An output string,
    parameters
    effects
    """

    def __init__(self, text, effects, parent=None):
        self.uuid = uuid1()
        #text :: str
        self.text = text
        #effects :: []
        self.effects = effects
        self.children = set()
        if parent is not None:
            self.parent = parent.uuid
            parent.push(self.uuid)
        else:
            self.parent = None
        #record of effect occurences
        self.record = defaultdict(lambda: 0)
        #number of times updated:
        self.__n = 0

    def push(self, uuid):
        self.children.add(uuid)

    def update(self, result):
        assert isinstance(result, Result)
        self.__n += 1
        for field in result:
            self.record[field] += result[field]

    def a_child(self):
        return choice([x for x in self.children])

    def __len__(self):
        return len(self.children)

#------------------------------------------------------------
class Result:
    """ Aggregated results of an action,
    Essentially a dict of the changes that occured,
    used to log the probabilites of each happening in a node
    """
    def __init__(self):
        self.data = {}

    def __iter__(self):
        return iter(self.data)

    def __getitem(self, key):
        return self.data[key]

#------------------------------------------------------------
if __name__ == "__main__":
    #any positive answer: clear the tree and start again
    clear_q = input('Clear?')
    if clear_q == '':
        tree = MCTSD_Tree()
    else:
        with open('./tree_backup','rb') as f:
            tree = pickle.load(f)
    tree.construct()

    #any positive answer: save the tree
    save_q = input('Save?')
    if save_q != '':
        with open('./tree_backup','wb') as f:
            pickle.dump(tree, f)
