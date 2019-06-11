import logging as root_logger
logging = root_logger.getLogger(__name__)


class Type:
    """ An unrestricted type """

    def __repr__(self):
        return "|∀|"


class TypeDefinition(Type):
    """ Can define Structure of a type """

    def __init__(self, name, path, structure):

        """ Structure creates the dict of locations.
        Only leaves get type anotations. Thus:
        { .a.$x :: String, .b.$c :: Num, .d!$e::Location }
        """
        #The name is the location. eg: .types.person
        self.name = name
        self.path = path
        self.structure = structure

    def __repr__(self):
        return "(Data: {})".format(self.name)

    def build_type_declaration(self):
        return MonoTypeVar(self.name, self.path)


class MonoTypeVar(Type):
    """ A MonoType Instance """
    TypeCounter = 0

    def __init__(self, name=None, path=None):
        #todo:
        if name is None:
            letter = ascii_lowercase[MonoTypeVar.TypeCounter % len(ascii_lowercase)]
            num = int(MonoTypeVar.TypeCounter / len(ascii_lowercase))
            name = "{}{}".format(letter,num)
            MonoTypeVar.TypeCounter += 1
        if path is None:
            path = name
        self.name = name
        self.path = path

    def __hash__(self):
        return hash("".join([str(x) for x in self.path]))

    def __repr__(self):
        return "(:: {})".format(self.name)

    def __eq__(self, other):
        #todo: match inheritance
        if not other:
            return False
        type_match = type(self) == type(other)
        name_match = self.name == other.name
        return type_match and name_match

    def __lt__(self, other):
        raise Exception("To be implemented")



