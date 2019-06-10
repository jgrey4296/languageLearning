
class TypeRedefinitionException(Exception):

    def __init__(self, typename):
        self.typename = typename

    def __str__(self):
        return "Type Redefinition Attempt: {}".format(self.typename)

    __repr__ = __str__

class TypeConflictException(Exception):

    def __init__(self, env_type, new_type, stmt):
        self.env_type = env_type
        self.new_type = new_type
        self.stmt = stmt

    def __str__(self):
        return "Exception: Expected {} but type has {} in {}".format(self.env_type,
                                                                     self.new_type,
                                                                     self.stmt)


    __repr__ = __str__

class TypeUndefinedException(Exception):

    def __init__(self, attempted_type, stmt):
        self.attempted_type = attempted_type
        self.stmt = stmt

    def __str__(self):
        return "Exception: Attempted to declare as missing type {} in {}".format(self.attempted_type,
                                                                                 self.stmt)

class TypeVariableConflictException(Exception):

    def __init__(self, node_path):
        self.node_path = node_path

    def __str__(self):
        return "Node specified as both a var and not a var: {}".format("".join(self.node_path))

class TypeStructureMismatch(Exception):

    def __init__(self, typename, conflicts):
        self.typename = typename
        self.conflicts = conflicts

    def __str__(self):
        return "{} Structure Mismatch: {}".format(self.typename,
                                                  ", ".join(self.conflicts))
