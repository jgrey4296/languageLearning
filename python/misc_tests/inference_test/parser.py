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
DOLLAR = s(pp.Literal('$'))
DBLCOLON = s(pp.Literal('::'))
COLON = s(pp.Keyword(':', ":"))
EQUAL = s(pp.Keyword('='))
VERT_LINE = s(pp.Keyword('|'))
OBRACKET = s(pp.Literal('['))
CBRACKET = s(pp.Literal(']'))
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
    path = data.SEN[:]
    baseName = path[-1]
    tvars = []
    if data.TVars:
        tvars = data.TVars[:]
    return TypeDefinition(baseName, path, data.Structure[:], tvars)

def makeTypeDec(data):
    path = data.SEN[:]
    baseName = path[-1]
    args = []
    if 'ARGS' in data:
        args = data.ARGS[:]
    elif baseName._type is not None:
        args.append(baseName._type)
        baseName._type = None
    return MonoTypeVar(baseName, path, args)

def makeRule(data):
    name = "".join([repr(x) for x in data.RuleName])
    return Rule(name, data.RuleName, data.Structure[:])

def makeWord(data):
    typedec = None
    if "TypeDec" in data:
        typedec = data.TypeDec[0]
    if 'CONST' in data:
        return ExConst(data[0], data[1], typedec)
    else:
        return ExVar(data[0], data[1], typedec)



# Standard Language
#todo: change NAME to include operators, then parse in type constructor
SENTENCE = pp.Forward()
TYPEDEC_CORE = pp.Forward()

CONST = NAME
VAR = DOLLAR + NAME

TYPEDEC_CORE << DBLCOLON + N("SEN", op(SENTENCE)) + N("ARGS", op(OPAR + pp.delimitedList(TYPEDEC_CORE, delim=', ', combine=False) + CPAR))

TYPEDEC = OPAR + TYPEDEC_CORE + CPAR

WORD = OP + pp.Or([VAR.setResultsName("VAR"),
                   CONST.setResultsName("CONST")])+ N("TypeDec", op(TYPEDEC))

SENTENCE << pp.OneOrMore(WORD)

TYPEALIAS = DBLCOLON + SENTENCE

VARLIST = OBRACKET + op(pp.delimitedList(VAR, delim=', ', combine=False)) + CBRACKET

TYPEDEF = DBLCOLON + N("SEN", SENTENCE) \
    + N("TVars", op(VARLIST)) + COLON  + opLn \
    + N("Structure", pp.ZeroOrMore(SENTENCE + pp.Or([COMMA, Ln]))) \
    + s(pp.Keyword("END"))

# TODO: Type constructor

RULE = N("RuleName", SENTENCE) + COLON + opLn \
    + N("Structure", pp.OneOrMore(SENTENCE + opLn)) + s(pp.Keyword("END"))

MAIN = s(op(pp.White(ws=" \t\n\r"))) + \
    pp.OneOrMore(pp.Or([TYPEDEF, RULE, SENTENCE]) + \
                 s(op(pp.White(ws=" \t\n\r"))))


# Actions
TYPEDEF.setParseAction(lambda t: makeTypeDef(t))
TYPEDEC_CORE.setParseAction(lambda t: makeTypeDec(t))
WORD.setParseAction(lambda t: makeWord(t))
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
