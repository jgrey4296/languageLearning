from random import choice, randint
import networkx as nx
from KahnSort import KahnSort as ks
from collections import defaultdict
import IPython
# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.ruleChain"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################


#Simple RuleChain Proof
primitives = list("abcdefghijklmnopqrstuvwxyz")
condition_limit = 3
action_limit = 3
rule_limit = (10, 20)

class RuleException(Exception):

    def __init__(self, conflicts):
        self.conflicts = conflicts

class Rule:
    id = 0

    def __init__(self, c=None, a=None):
        self.id = Rule.id
        Rule.id += 1
        self.conditions = set()
        self.actions = set()
        if c is not None:
            self.conditions = set(c)
        if a is not None:
            self.actions = set(a).difference(c)

    def is_valid(self):
        return bool(self.conditions) and bool(self.actions)

    def __repr__(self):
        return "({}:{} -> {})".format(self.id, ",".join(self.conditions),
                                      ",".join(self.actions))

def createGraph(rules):
    """ Link Rules by conditions -> actions """
    graph = nx.DiGraph()
    for r in rules:
        for x in r.conditions:
            for y in r.actions:
                graph.add_edge(x,y)
    return graph


def createRule():
    conds = [choice(primitives) for x in range(randint(1, condition_limit))]
    acts = [choice(primitives) for x in range(randint(1, action_limit))]
    newRule = Rule(c=conds, a=acts)
    return newRule

def createRules():
    rules = [createRule() for x in range(randint(*rule_limit))]
    return [r for r in rules if r.is_valid()]

def constructKeyedMap(rules, key=None):
    #key = lambda r: r.conditions | r.actions
    assert(key is not None)
    #condition_map :: { primitive : set(Rule) }
    keyedMap = defaultdict(lambda: set())
    for r in rules:
        for x in key(r):
            keyedMap[x].add(r)
    return keyedMap



if __name__ == "__main__":
    rules = createRules()
    rule_map = { x.id : x for x in rules }
    cond_map = constructKeyedMap(rules, lambda r: r.conditions)
    act_map = constructKeyedMap(rules, lambda r: r.actions)
    rule_graph = createGraph(rules)
    IPython.embed(simple_prompt=True)
    try:
        ruleLayers = ks.sort(rule_graph, set(["a"]))
    except RuleException as e:
        conflicts = e.conflicts
