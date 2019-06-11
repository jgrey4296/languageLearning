import logging as root_logger
logging = root_logger.getLogger(__name__)

class BasicNode:

    def __init__(self):
        self.name = "__root"
        self._type = None

    def __repr__(self):
        return "(root)"

    def is_var(self):
        return False



#--------------------------------------------------

def has_equivalent_vars_pred(node):
    """ A Predicate to use with Trie.get_nodes
    Finds nodes with multiple vars as children that can be merged """
    if node.name == "_root":
        return False
    var_children = [x for x in node._children.values() if x.is_var]
    return len(var_children) > 1
