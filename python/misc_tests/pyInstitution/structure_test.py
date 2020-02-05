import IPython
import networkx as nx
from uuid import uuid1
from random import choice


#Generate a tree structure organisation
first_names = "Carrot Angua Nobby Fred Sam Cheery Visit".split(' ')
sur_names   = "Ironfoundersson Nobbs Colon Vimes Littlebottom the_Infidels_with_explanatory_pamphlets".split(' ')

def genName():
    return "{} {}".format(choice(first_names),choice(sur_names))

flat_t_p = ['captain', 2, 'sergeant', 3, 'constable', 4]

class Organisation:
    """ Representation of the *formal* structure of a hierarchical organisation """

    def __init__(self,nameGenFunc,flat_titles_and_nums,root_title):
        #TODO: add Title generation, remove nameGen,
        #TODO: further structure the tree
        #TODO: Add variations that aren't trees. Stars, Rings, Complex. any others?

        self.genName = nameGenFunc
        #Position titles and branch factors
        self.structure_definition = list(zip(flat_titles_and_nums[0::2],flat_titles_and_nums[1::2]))

        #The connections between positions
        self.graph = nx.DiGraph()
        #The leader:
        self.root = (uuid1(), root_title)
        #All the different positions, grouped
        self.position_groups = {}

        #TODO: move this to the *entity*
        #Who *holds* a particular position. posId -> Individual
        self.positions = {}

        #Additional data:
        self.action_primitives = []
        self.behaviour_schemas = []
        self.regulations       = [] #who can do what
        self.concepts          = [] #the things counts-as generate, resources, etc. components of rules.
        self.counts_as_rules   = [] #constitutive rules
        self.value_hierarchy   = []

        self.gen_structure()

    def gen_structure(self):
        """ Use the org structure definition to create the position tree, and fill it with people """
        self.graph.add_node(self.root[0],title=self.root[1])
        self.position_groups[self.root[1]] = [self.root[0]]
        self.positions[self.root[0]] = self.genName()

        current = [(0,self.root[0])]
        while len(current) > 0:
            depth, id = current.pop()
            try:
                title,num = self.structure_definition[depth]
                if title not in self.position_groups:
                    self.position_groups[title] = []

                unique_titles = [(uuid1(),title) for x in range(num)]
                for unique in unique_titles:
                    self.graph.add_node(unique[0],title=unique[1])
                    self.graph.add_edge(id,unique[0])
                    self.position_groups[title].append(unique[0])
                    self.positions[unique[0]] = self.genName()
                current.extend([(depth+1,id) for id,name in unique_titles])
            except IndexError:
                continue


    def add_position(self,superiors,subordinates):
        None

    def remove_position(self,id):
        None

    #TODO: add/remove_(action, behaviour, regulation, concept, count_as, value)
    #TODO: machine learning of value preferences?

def Entity:
    """ The representation of the *actual* institution """
    def __init__(self,org,nameGen):
        self.org = org
        self.interaction_graph = nx.DiGraph() #Likely a multiDiGraph
        self.agents = {}    #all the actual agents
        self.positions = {} #to be moved from the org

        self.norms = []    #Non-Formal Rules and value modifications of the formal structure


    def demote(self,id):
        """ Get the individual, remove them from the position and place them on a lower position
        Choose the next available free position, or create if none are free"""
        None

    def promote(self,id):
        """ Get an individual, remove them from the current position, and place them on a higher position
        Choose the next available free position, or create if none are free """
        None

    def remove_individual(self,id):
        """ Remove an individual entirely """
        None

    def add_individual(self,id,position):
        None


if __name__ == "__main__":
    anOrg = Organisation(genName,flat_t_p,'commander')
    IPython.embed(simple_prompt=True)
