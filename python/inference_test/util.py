from enum import Enum
from String import ascii_lowercase

ExOp = Enum('Exclusion Operator', 'DOT EX')

class Term:
    pass

class ExConst(Term):
    "A Constant: .something  and  !something """

    def __init__(self, ex, value, type_an=None):
        self.name = value
        self.ex = ex
        self.type_annotation = type_an

    def __str__(self):
        type_str = ""
        exstr = "."
        if self.ex == ExOp.EX:
            exstr = "!"
        if self.type_annotation != None:
            type_str = "( :: {})".format(self.type_annotation)
        return "{}{}{}".format(exstr, self.name, type_str)

    def __eq__(self, other):
        type_match = type(self) == type(other)
        type_annotation_match = True
        if self.type_annotation and other.type_annotation:
            type_annotation_match = self.type_annotation == other.type_annotation
        val_match = self.name == other.name
        ex_match = self.ex == other.ex
        return all([type_match, val_match, ex_match, type_annotation_match])

    __repr__ = __str__

class ExVar(ExConst):

    def __str__(self):
        type_str = ""
        exstr = "."
        if self.ex == ExOp.EX:
            exstr = "!"
        if self.type_annotation != None:
            type_str = "( :: {})".format(self.type_annotation)
        return "{}${}{}".format(exstr, self.name, type_str)

    __repr__ = __str__

#--------------------------------------------------
class Type:
    pass

#TODO Types: string, query, rule, cycle, layer, agenda, curve
# OP, Action, Input, Sets (recovery actions...?, FSMs, Tag...)

class MonoTypeVar(Type):
    TypeCounter = 0


    def __init__(self, name=None):
        #todo:
        if name is None:
            letter = ascii_lowercase[TypeCounter % len(ascii_lowercase)]
            num = int(TypeCounter / len(ascii_lowercase))
            name = "{}{}".format(letter,num)
            TypeCounter += 1
        self.name = name

    def __str__(self):
        return "(:: {})".format(self.name)

    __repr__ = __str__

    def __eq__(self, other):
        #todo: match inheritance
        type_match = type(self) == type(other)
        name_match = self.name == other.name
        return type_match and name_match

    def __lt__(self, other):
        raise Exception("To be implemented")



