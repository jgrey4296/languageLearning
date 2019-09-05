from enum import Enum
from string import ascii_lowercase
from util import BasicNode
import IPython
import type_exceptions as te
import logging as root_logger
logging = root_logger.getLogger(__name__)

ExOp = Enum('Exclusion Operator', 'DOT EX')

# Value Data
class Term(BasicNode):
    """ Base Value """
    def __init__(self):
        self._type = None
        pass

    def printType(self):
        return str(self._type)

    def __repr__(self):
        return "0"

    def is_var(self):
        return False


class ExConst(Term):
    """A Constant: .something  and  !something """

    def __init__(self, ex, value, type_an=None):
        self.name = value
        self.ex = ex
        self._type = type_an

    def __repr__(self):
        type_str = ""
        exstr = "."
        if self.ex == ExOp.EX:
            exstr = "!"
        if self._type != None:
            type_str = repr(self._type)
        return "{}{}{}".format(exstr, self.name, type_str)

    def __str__(self):
        return self.name

    def __eq__(self, other):
        type_match = type(self) == type(other)
        type_annotation_match = True
        if self._type and other._type :
            type_annotation_match = self._type == other._type
        val_match = self.name == other.name
        ex_match = self.ex == other.ex
        return all([type_match, val_match, ex_match, type_annotation_match])


class ExVar(ExConst):
    """ A Variable: .$x and !$x """

    def __repr__(self):
        type_str = ""
        exstr = "."
        if self.ex == ExOp.EX:
            exstr = "!"
        if self._type != None:
            type_str = str(self._type)
        return "{}${}{}".format(exstr, self.name, type_str)

    def __str__(self):
        return self.name

    def abs_var(self):
        exstr = "."
        type_str = ""
        if self.ex == ExOp.EX:
            exstr = "!"
        if self._type != None:
            type_str = str(self._type)
        return "{}$_{}".format(exstr, type_str)

    def type_it(self, the_type):
        if self._type != None:
            raise te.TypeConflictException(self._type, the_type, self)

        return ExVar(self.ex, self.name, the_type)

    def is_var(self):
        return True


class Rule(Term):
    """ A Simplified Rule: consists of a list of sentences """

    def __init__(self, name, path, struct):
        self.name = name
        self.path = path
        self.struct = struct
        self._type = "Rule"

    def __repr__(self):
        return "Rule: {}".format(repr(self.name))

    def __str__(self):
        return "{}:\n{}\nEND".format(self.name,
                                     "\n".join([
                                         "    {}".format("".join([str(y) for y in x]))
                                         for x in self.struct]))


