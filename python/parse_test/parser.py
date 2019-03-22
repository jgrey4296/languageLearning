# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
logFileName = "parsing.log"
root_logger.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
####################
import ply.lex as lex
import ply.yacc as yacc


#TOKENS
reserved = {
    'if' : 'IF'

    }


tokens = ( 'NAME', 'NUMBER',
           'PLUS', 'MINUS',
           'LPAREN', 'RPAREN',
           'EQUALS'
) + list(reserved.values())


#LEXICAL PATTERNS
t_PLUS = r'\+'
t_MINUS = r'\-'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_EQUALS = r'='

t_NAME = r'[a-zA-Z_][a-zA-Z0-0_]*'

def t_NUMBER(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        logging.info("Integer too large {}".format(t.value))
        t.value = 0
    return t

#ignored characters:
t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    logging.info("Illegal character {}".format(t.value[0]))
    t.lexer.skip(1)

#Parse rules:
precedence = (
    ('left','PLUS','MINUS'),
    ('right','UMINUS')
    )

#Dictionary of names
names = {}

def p_statement_assign(t):
    'statement : NAME EQUALS expression'
    names[t[1]] = t[3]

def p_statement_expr(t):
    'statement : expression '
    print(t[1])

def p_expression_binop(t):
    ''' expression : expression PLUS expression
                   | expression MINUS expression '''
    if t[2] == '+' : t[0] = t[1] + t[3]
    elif t[2] == '-' : t[0] = t[1] - t[3]

def p_expression_uminus(t):
    'expression : MINUS expression %prec UMINUS'
    t[0] = -t[2]

def p_expression_group(t):
    'expression : LPAREN expression RPAREN'
    t[0] = t[2]

def p_expression_number(t):
    'expression : NUMBER'
    t[0] = t[1]

def p_expression_name(t):
    'expression : NAME'
    try:
        t[0] = names[t[1]]
    except LookupError:
        logging.info("Undefined name {}".format(t[1]))
        t[0] = 0

def p_error(t):
    logging.info("Syntax error at {}".format(t.value))


lexer = lex.lex()
parser = yacc.yacc()        
while True:
    try:
        s = input('calc >')
    except EOFError:
        break
    parser.parse(s)
