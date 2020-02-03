import networkx as nx


class Organisation:
    def __init__(self):
        self.action_primitives = []
        self.behaviour_schemas = []
        self.regulations       = []
        self.constitutives     = []
        self.roles             = []
        self.formal_structure  = nx.DiGraph()



class Environment:
    def __init__(self):
        self.artifacts = []
        self.processes = []


class Entity:
    def __init__(self,org):
        self.interaction_structure = nx.DiGraph()
        self.agents = []
        self.role_assignments = []
        self.formal_org = org

class Agent:
    def __init__(self):
        self.observed_structure = nx.DiGraph()
        self.knowledge = []


#Types of tool
class Tool:
    """ Simple tool, you use it, something happens """
    def __init__(self,name,verb,effects):
        self.name = name
        self.verb = verb
        self.effects = effects

    def __call__(self,actor):
        """ Eg: Bob uses the razor """
        return ("{} {} the {}".format(actor, self.verb, self.name),
                [x(actor) for x in self.effects])

class Consuming_Tool(Tool):
    """ A Tool where you use it, it consumes an item, causing an effect """
    def __init__(self,name,verb,effects):
        super().__init__(name,verb,effects)

    def __call__(self,actor,target):
        """ Eg: Bob uses the cutlery to eat the carrot """
        return ("{} uses the {} to {} the {}".format(actor,self.name,self.verb,target),
                [x(actor,target) for x in self.effects])

class Production_Tool(Tool):
    """ A Tool where you use it, and it produces a new item """
    def __init__(self,name,verb,effects,production_item):
        super().__init__(name,verb,effects)
        self.production_item = production_item

    def __call__(self,actor):
        return ("{} {} {} with the {}".format(actor,self.verb,self.production_item,self.name),
                [x(actor,self.production_item) for x in self.effect])

class Transformation_Tool(Tool):
    """ A Tool that consumes a set of items, and produces another """
    def __init__(self,name,verb,effects,consumption_set,production_set):
        super().__init__(name,verb,effects)
        self.consumption_set = consumption_set
        self.production_set = production_set

    def __call__(self,actor)
