"""
A Simpler parser to infer types on
"""
from util import ExOp, ExConst, ExVar, MonoTypeVar
import pyparsing as pp
import logging as root_logger
import IPython
logging = root_logger.getLogger(__name__)

#Defaults
s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.lineEnd))
comment = s(pp.dblSlashComment)

NAME = pp.Word(pp.alphas)
DOLLAR = s(pp.Literal('$'))
DBLCOLON = s(pp.Literal('::'))
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

# Standard Language

TYPE = OPAR + DBLCOLON + NAME + CPAR
CONST = OP + NAME + op(TYPE)
VAR = OP + DOLLAR + NAME + op(TYPE)

WORD = pp.Or([VAR, CONST])

SENTENCE = pp.OneOrMore(WORD)

TYPE.setParseAction(lambda t: MonoTypVar(t[0]))
CONST.setParseAction(lambda t: ExConst(*t))
VAR.setParseAction(lambda t: ExVar(*t))

def parseString(in_string):
    try:
        return SENTENCE.parseString(in_string)
    except pp.ParseException as x:
        print("Error:\nLine {}, column {}:\n'{}'".format(x.lineno,
                                                         x.col,
                                                         x.markInputline()))
        IPython.embed(simple_prompt=True)
