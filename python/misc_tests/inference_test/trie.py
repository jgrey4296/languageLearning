"""
A Trie for Types
"""
import type_exceptions as te
from util import BasicNode
from terms import ExVar
import IPython
from string import ascii_lowercase
import logging as root_logger
logging = root_logger.getLogger(__name__)


log_messages = {}
log_messages['validate_top'] = "Validating: {} on {} ({})"
log_messages['curr_def'] = "Current Definition to Validate: {} : {} : {}"
log_messages['curr_use_set'] = "Current Usage Set: {}"
log_messages['no_children'] = "Val: No Children, assigning type: {} to {}"
log_messages['match_type_usage'] = "Matching Type {} onto usage set"
log_messages['mult_child'] = "Current Def has multiple children, checking for conflicts in structure"



class TrieNode:

    def __init__(self, node, path):
        self.name = node.name
        self.path = path[:]
        self._type = None
        self._children = {}
        self.data = None
        self.references = {}

    def print_path(self):
        result = "{} | {}".format(" ".join([repr(x) for x in self.path]),
                                  repr(self))
        return result

    def __repr__(self):
        return "TN: {}".format(self.name)

    def __hash__(self):
        return hash(str(self))

    def __len__(self):
        return len(self._children)

    def __bool__(self):
        return bool(self._children)

    def add_child(self, node):
        self._children[node.name] = node
        return node

    def get_child(self, node):
        return self._children[node.name]

    def has_child(self, node):
        return node.name in self._children

    def update(self, info):
        return None

    def set_data(self, data):
        logging.debug("Setting Data: {}".format(data))
        self.data = data


class TypeDefTrieNode(TrieNode):

    def __init__(self, node, path):
        logging.debug("TypeDefTrieNode: init: {} / {}".format(node, path))
        super().__init__(node, path)
        self._typedef_trie = None

    def __repr__(self):
        return "TypeDefTrieNode: {}".format("".join([repr(x) for x in self.path]))

    def set_data(self, data):
        logging.debug("TypeDef.set_data: {}".format(data))
        if self.data is None:
            self.data = data
            #construct the internal trie
            self._typedef_trie = Trie(node_type=TypeAssignmentTrieNode)
            self._typedef_trie._root._type = self.data.build_type_declaration()
            for x in self.data.structure:
                self._typedef_trie.add(x, None,
                                       update=lambda c, n, p, d: c.type_match_wrapper(n))
            return

        if self.data != data:
            raise te.TypeRedefinitionException(self.data)

    def validate(self, usage_trie):
        usage_path = "".join([repr(x) for x in usage_trie.path])
        logging.debug(log_messages['validate_top'].format(repr(self),
                                                          repr(usage_trie),
                                                          usage_path))
        assert(isinstance(usage_trie, TrieNode))
        if self._typedef_trie is None:
            raise TypeUndefinedException(self.name, usage_trie)

        type_var_lookup = {}
        if self.data._vars and usage_trie._type and usage_trie._type._args:
            zipped = zip(self.data._vars, usage_trie._type._args)
            type_var_lookup = { x : y for x,y in zipped }


        # Loop over all elements of the defined type
        newly_typed = []
        queue = [(self._typedef_trie._root, [usage_trie])]
        while queue:
            curr_def, curr_usage_set = queue.pop(0)
            curr_def_type = curr_def._type
            curr_use_str = ", ".join([str(x) for x in curr_usage_set])
            curr_def_path = "".join([repr(x) for x in curr_def.path])
            logging.debug(log_messages['curr_def'].format(curr_def_path,
                                                          curr_def.name,
                                                          repr(curr_def_type)))
            logging.debug(log_messages['curr_use_set'].format(curr_use_str))
            #use stored type variables if on a typedef variable node
            if curr_def.is_var and curr_def.name in type_var_lookup:
                curr_def_type = type_var_lookup[curr_def.name]
            elif curr_def._type is not None:
                curr_def_type = curr_def_type.build_type_declaration(type_var_lookup)

            # if you are at the root, check you aren't
            #a variable reference to elsewhere
            if curr_def.name != "__root":
                for x in curr_usage_set:
                    if not x.is_var and curr_def.name != x.name:
                        raise te.TypeStructureMismatch(curr_def.name, [x.name])

            #if there are no children, assign a type and finish
            if not bool(curr_def) and curr_def_type is not None:
                c_u_s_str = ", ".join([str(x) for x in curr_usage_set])
                logging.debug(log_messages['no_children'].format(curr_def_type,
                                                                 c_u_s_str))
                type_attempts = [x.type_match(curr_def_type) for x in curr_usage_set]
                newly_typed += [x for x in type_attempts if x is not None]

            #if there is one child, match all usage set against it
            elif len(curr_def) == 1:
                logging.debug("Curr Def has a single child")
                child = list(curr_def._children.values())[0]
                if curr_def_type is not None:
                    log_msg = log_messages['match_type_usage'].format(curr_def_type)
                    logging.debug(log_msg)
                    type_attempts = [x.type_match(curr_def_type) for x in curr_usage_set]
                    newly_typed += [x for x in type_attempts if x is not None]

                new_usage_set = [y for x in curr_usage_set for y in x._children.values()]
                if bool(new_usage_set):
                    queue.append((child, new_usage_set))

            else: #curr_def._children > 1
                logging.debug(log_messages['mult_child'])
                # With multiple children, match keys
                defset = { x for x in curr_def._children.keys() }
                usageset = { y for x in curr_usage_set for y,n in x._children.items() if not n.is_var }
                conflicts = usageset.difference(defset)
                if bool(conflicts):
                    raise te.TypeStructureMismatch(curr_def.path,
                                                   conflicts)
                logging.debug("No Conflicts found, checking children")
                for key in usageset:
                    new_usage_set = [x._children[key] for x in curr_usage_set if key in x._children]
                    new_child = curr_def._children[key]
                    if bool(new_usage_set):
                        queue.append((new_child, new_usage_set))
            logging.debug("----------")
        return newly_typed


class M_TypedNode(TrieNode):

    def type_match_wrapper(self, node):
        if node._type is None:
            return
        self.type_match(node._type)

    def type_match(self, _type):
        if self._type is None:
            self._type = _type
            return self
        elif self._type != _type:
            raise te.TypeConflictException(self._type,
                                           _type,
                                           self.path)
        return None


class TypeAssignmentTrieNode(M_TypedNode):

    def __init__(self, node, path):
        super().__init__(node, path)
        self._type = node._type
        self.is_var = node.is_var()
        self.var_node = None

    def __repr__(self):
        type_str = ""
        if self._type is not None:
            type_str = repr(self._type)
        return "TA: {} {}".format(repr(self.name), type_str)

    def update(self, node, lookup):
        logging.debug("Node: {} updating with {}".format(self.name,
                                                        node.name))
        self.type_match_wrapper(node)
        if not self.is_var and node.is_var():
            raise te.TypeVariableConflictException(self.path)

        if self.is_var and self.var_node is None:
            self.var_node = lookup.add([self.path[-1]], [])
            self.var_node.nodes.add(self)
            self.var_node.add_var_name(node)
            self.var_node.type_match_wrapper(node)

    def has_child(self, node):
        return node.name in self._children

    def get_child(self, node):
        return self._children[node.name]


class VarTypeTrieNode(M_TypedNode):

    def __init__(self, node, path):
        super().__init__(node, path)
        self._type = node._type
        self.nodes = set([])
        self.var_names = set([])

    def __repr__(self):
        if self._type is not None:
            return "VT: {}".format(repr(self._type))
        else:
            return "VT: 0"

    def add_var_name(self, node):
        if node.is_var():
            self.var_names.add(node.name)

    def add_node(self, node):
        self.nodes.add(node)
        node.is_var = True
        node.var_node = self

    def propagate(self):
        if self._type is not None:
            for n in self.nodes:
                n.type_match(self._type)

    def merge(self, nodes):
        assert(all([isinstance(x, VarTypeTrieNode) for x in nodes]))
        logging.debug("Merging Variables: {} into {}".format(", ".join([x.name for x in nodes]), self.name))
        #Get the set of all types for the variables
        all_types = {x._type for x in nodes if x._type is not None}
        # update self to point to all assignment nodes
        [self.add_node(y) for x in nodes for y in x.nodes]
        [self.type_match(x) for x in all_types]
        self.propagate()


class Trie:

    def __init__(self, node_type=TrieNode):
        self._root = node_type(BasicNode(), [".root"])
        self.node_type = node_type

    def __repr__(self):
        return "Trie: {}".format(len(self.get_nodes()))

    def query(self, path):
        current = self._root
        for x in path:
            logging.debug("Searching {} for: {}".format(str(current), x))
            if current.has_child(x):
                current = current.get_child(x)
            else:
                return None
        return current

    def add(self, path, data, update=None, u_data=None):
        current = self._root
        current_path = []
        for x in path:
            current_path.append(x)
            if current.has_child(x):
                current = current.get_child(x)
                logging.debug("Trie: Retrieved: {}".format(current))
            else:
                current = current.add_child(self.node_type(x, current_path))
                logging.debug("Trie: Added: {}".format(current))
            if update is not None:
                update(current, x, current_path, u_data)

        current.set_data(data)

        return current

    def remove(self, path):
        query = self.query(path[:-1])
        if query is not None and path[-1].name in query._children:
            del query._children[path[-1].name]

    def get_nodes(self, pred=None):
        nodes = []
        queue = [self._root]
        visited = set()
        while queue:
            current = queue.pop(0)
            visited.add(current)
            if (pred is None or pred(current)) and current not in nodes:
                nodes.append(current)
            queue += [x for x in list(current._children.values()) if x not in visited]

        return nodes

    def __len__(self):
        return len(self.get_nodes(lambda x: not bool(x)))

    def print_trie(self):
        leaves = self.get_nodes(lambda x: not bool(x._children))
        leaf_paths = [x.print_path() for x in leaves]
        return "\n".join(leaf_paths)
