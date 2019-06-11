"""
A Simpler parser to infer types on
"""
from terms import ExOp, ExConst, ExVar, Rule
from ex_types import MonoTypeVar, TypeDefinition
from collections import namedtuple
import pyparsing as pp
import logging as root_logger
import IPython
logging = root_logger.getLogger(__name__)
pp.ParserElement.setDefaultWhitespaceChars(' \t\r')

#Defaults
s = pp.Suppress
op = pp.Optional
Ln = s(pp.lineEnd)
opLn = op(Ln)
comment = s(pp.dblSlashComment)

NAME = pp.Word(pp.alphas)
LATER_PARSE_SEN = pp.Word(pp.alphas + ".!$")
DOLLAR = s(pp.Literal('$'))
DBLCOLON = s(pp.Literal('::'))
COLON = s(pp.Literal(':'))
EQUAL = s(pp.Keyword('='))
VERT_LINE = s(pp.Keyword('|'))
OBRACKET = s(pp.Keyword('['))
CBRACKET = s(pp.Keyword(']'))
COMMA = s(pp.Keyword(','))
ARROW = s(pp.Keyword('->'))
OPAR = s(pp.Literal('('))
CPAR = s(pp.Literal(')'))

DOT = pp.Keyword('.', identChars='!').setParseAction(lambda t: ExOp.DOT)
EX = pp.Keyword('!', identChars='.').setParseAction(lambda t: ExOp.EX)
OP = pp.Or([DOT, EX])

def NG(name, grp):
    """ Name and Group """
    return pp.Group(grp).setResultsName(name)

def N(name, parser):
    return parser.setResultsName(name)

#Ctor utilities
def makeTypeDef(data):
    #convert sentences of structure to name : typeOfLeaf
    path = safeParse(SENTENCE, ".{}{}".format(data.TypeName[0],
                                              data.SEN))[0]
    baseName = path[-1]

    return TypeDefinition(baseName, path, data.Structure)

def makeTypeDec(data):
    path = safeParse(SENTENCE, ".{}{}".format(data[0], data.SEN))[0]
    baseName = path[-1]
    return MonoTypeVar(baseName, path)

def makeRule(data):
    name = "".join([str(x) for x in data.RuleName[0]])
    return Rule(name, data.Structure[:])

def makeConst(data):
    return ExConst(*data)


# Standard Language
#todo: change NAME to include operators, then parse in type constructor
TYPEDEC = OPAR + DBLCOLON + NAME + N("SEN", op(LATER_PARSE_SEN)) + CPAR
CONST = OP + NAME + N("TypeDec", op(TYPEDEC))
VAR = OP + DOLLAR + NAME + N("TypeDec", op(TYPEDEC))

WORD = pp.Or([VAR, CONST])

SENTENCE = pp.OneOrMore(WORD)

TYPEALIAS = DBLCOLON + NAME + op(SENTENCE) + TYPEDEC

TYPEDEF = DBLCOLON + NG("TypeName", NAME) + N("SEN", op(LATER_PARSE_SEN)) + COLON \
    + opLn + NG("Structure", pp.ZeroOrMore(SENTENCE + Ln)) + s(pp.Keyword("END"))

# TODO: Type constructor

RULE = NG("RuleName", SENTENCE) + COLON + opLn \
    + NG("Structure", pp.OneOrMore(SENTENCE + opLn)) + s(pp.Keyword("END"))

MAIN = s(op(pp.White(ws=" \t\n\r"))) + \
    pp.OneOrMore(pp.Or([TYPEDEF, RULE, TYPEALIAS, SENTENCE]) + \
                 s(op(pp.White(ws=" \t\n\r"))))


# Actions
TYPEDEF.setParseAction(lambda t: makeTypeDef(t))
TYPEDEC.setParseAction(lambda t: makeTypeDec(t))
CONST.setParseAction(lambda t: makeConst(t))
VAR.setParseAction(lambda t: ExVar(*t))
RULE.setParseAction(lambda t: makeRule(t))
SENTENCE.setParseAction(lambda t: [t[:]])

def printTypes(lst):
    return [x.printType() for x in lst]

def safeParse(parser, string):
    try:
        return parser.parseString(string)
    except pp.ParseException as x:
        print("Error:\nLine {}, column {}:\n'{}'".format(x.lineno,
                                                         x.col,
                                                         x.markInputline()))
        IPython.embed(simple_prompt=True)

def parseString(in_string):
    return safeParse(MAIN, in_string)
