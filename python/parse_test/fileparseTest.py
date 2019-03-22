# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "fileParseTest.log"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################
from pyparsing import Word, alphas, OnlyOnce, Keyword, Literal, ZeroOrMore, OneOrMore
from pyparsing import pyparsing_common as ppc
# OnlyOnce, ParseResults, FollowedBy, Forward, NotAny, OneOrMore, ZeroOrMore, Optional, SkipTo,
# Combine, Dict, Group, Suppress,
# And, Each, MatchFirst, Or, CharsNotIn, Empty, Keyword, CaselessKeyword, Literal, CaselessLiteral,
# NoMatch, QuotedString, Regex, White, Word

from os.path import join,isfile,exists,isdir
from os import listdir

INPUT = "testFile"

#r/w/rb/wb
with open(INPUT,'r') as f:
     inputText = f.read()

logging.info("Read text  : {}".format(inputText))

OPAR = Literal('(')
CPAR = Literal(')')
PARENS = OPAR | CPAR

EQUATION = OPAR + Literal('+') + ppc.integer + ppc.integer + CPAR
EQUATION.setParseAction(lambda toks: int(toks[2]) + int(toks[3]))

LIST =  OPAR + Literal('this') + ZeroOrMore(Word(alphas) | EQUATION) + CPAR
LIST.setParseAction(lambda toks: [[x for x in toks if not PARENS.matches(x)]])

parser = OneOrMore(LIST)

logging.info("Parsed Text: {}".format(parser.parseString(inputText)))
