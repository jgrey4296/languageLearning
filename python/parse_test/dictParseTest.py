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
import pyparsing as pp
from pyparsing import pyparsing_common as ppc
# OnlyOnce, ParseResults, FollowedBy, Forward, NotAny, OneOrMore, ZeroOrMore, Optional, SkipTo,
# Combine, Dict, Group, Suppress,
# And, Each, MatchFirst, Or, CharsNotIn, Empty, Keyword, CaselessKeyword, Literal, CaselessLiteral,
# NoMatch, QuotedString, Regex, White, Word

from os.path import join,isfile,exists,isdir
from os import listdir

INPUT = "test_file_dict"

#r/w/rb/wb
with open(INPUT,'r') as f:
     inputText = f.read()

logging.info("Read text  : {}".format(inputText))

#Syntax you don't care about:
OBRACE = pp.Suppress(pp.Literal('{'))
CBRACE = pp.Suppress(pp.Literal('}'))
OPAR = pp.Suppress(pp.Literal('('))
CPAR = pp.Suppress(pp.Literal(')'))
COLON = pp.Suppress(pp.Literal(':'))
COMMA = pp.Suppress(pp.Literal(','))
OBRACKET = pp.Suppress(pp.Literal('['))
CBRACKET = pp.Suppress(pp.Literal(']'))

#A Name for an assignment, ie: NAME : array
NAME = pp.Word(pp.alphas)
NAME = NAME.setResultsName('name')

#The Array of an assignment, ie: name : [1,2,3,4]
#Commas and Brackets ignored
ARRAY = OBRACKET + pp.ZeroOrMore(ppc.integer + pp.ZeroOrMore(COMMA + ppc.integer)) + CBRACKET
#Convert the integer strings to actual numbers
ARRAY.setParseAction(lambda toks: [[int(x) for x in toks if ppc.integer.matches(x)]])
ARRAY = ARRAY.setResultsName('array')

#The grouped assignment, ie: (NAME, ARRAY), the colon is ignored
ASSIGNMENT = pp.Group(NAME + COLON + ARRAY)

#An object, ie: { (Name : Array)* }
OBJ = OBRACE + pp.OneOrMore(ASSIGNMENT + pp.Optional(COMMA)).setResultsName('assignments') + CBRACE

#Parse multiple objects:
parser = pp.OneOrMore(pp.Group(OBJ))
parser = parser.setResultsName('objs')
#use the parser:
results = parser.parseString(inputText)
logging.info("Parsed Text: {}".format(results))

#Deprecated but useful to remember:
# def convertToObject(toks):
#     toks_minus_commas = [x for x in toks if not COMMA.matches(x) and not BRACES.matches(x)]
#     logging.info("TOKS_MINUS_COMMAS: {}".format(toks_minus_commas))
#     return {"obj_{}".format(x):y for x,y in toks_minus_commas}    
#OBJ.setParseAction(convertToObject)
