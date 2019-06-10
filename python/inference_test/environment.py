from ex_types import TypeDefinition
from terms import ExConst
import trie as T
import type_exceptions as te
import logging as root_logger
import IPython
logging = root_logger.getLogger(__name__)


class Environment:

    def __init__(self, init_types):
        # lookup trie of types
        self.type_equations = T.Trie(node_type=T.TypeDefTrieNode)

        # trie of current types
        self.type_assignments = T.Trie(node_type=T.TypeAssignmentTrieNode)
        # variable types
        self.var_types = T.Trie(node_type=T.VarTypeTrieNode)
        # validation queue
        self.validation_queue = []

        for x in init_types:
            self.add(x)

    def __repr__(self):
        return "Environment: Vars: ({}), Equ: {}, Assn: {}".format(len(self.var_types),
                                                                   len(self.type_equations),
                                                                   len(self.type_assignments))

    def var_query(self, other):
        queried = self.var_types.query(other)
        return queried

    def validate(self):
        val_queue = set()
        #merge equivalent variables
        # { str_path : [] }
        equivalent = {}


        #Set up the Base Environment
        for x in self.var_types.get_nodes(lambda x: x._type is not None):
            x.propagate()
            val_queue.update(x.nodes)

        val_queue.update({y for y in self.type_assignments.get_nodes(lambda x: x._type is not None)})

        dealt_with = set()
        #Now Loop over known types, to check and infer unknown types
        while bool(val_queue):
            head = val_queue.pop()
            if head in dealt_with:
                continue
            dealt_with.add(head)
            #check the head
            head_type = self.type_equations.query(head._type.path)
            if head_type is None:
                raise te.TypeUndefinedException(head._type, head)

            newly_typed = head_type.validate(head)
            for x in newly_typed:
                val_queue.add(x)
                if x.is_var:
                    x.var_node.type_match(x._type)
                    val_queue.update(x.var_node.nodes)

        return True

    def add(self, other):
        logging.debug("Environment: Adding {}".format(other))
        if isinstance(other, TypeDefinition):
            self.add_typedef(other)
            return
        #if op: add_op

        #----------
        # Type Assignment:
        self.add_sentence(other)

    def add_typedef(self, typedef):
        self.type_equations.add(typedef.path, typedef)

    def add_sentence(self, lst):
        #add the sentence to the type assignment trie
        self.type_assignments.add(lst, None,
                                  update=lambda c, n, p, d: c.update(n, d),
                                  u_data=self.var_types)

    def add_operations(self, op):
        return True
