"""
Get versu files from data dir,

output to similarly named files in analysis directory
"""
import IPython
import utils
from enum import Enum
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir
from random import shuffle
import pyparsing as pp

# Setup root_logger:
from os.path import splitext, split
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################

versu_e = Enum('Versu Enums', 'COMMENT COPEN CCLOSE ODEF CDEF TOPEN END')

class VersuType(utils.ParseBase):

    def __init__(self, name):
        super().__init__()
        self._type = "type"
        self._name = name

class VersuProcess(utils.ParseBase):

    def __init__(self, name):
        super().__init__()
        self._type = "process"
        self._name = name

class VersuFunction(utils.ParseBase):

    def __init__(self, name):
        super().__init__()
        self._type = "function"
        self._name = name

class VersuCall(utils.ParseBase):

    def __init__(self, name):
        super().__init__()
        self._type = "call"
        self._name = name


class VersuAction(utils.ParseBase):

    def __init__(self, name):
        super().__init__()
        self._type = "action"
        self._name = name

class VersuState(utils.ParseBase):

    def __init__(self, name):
        super().__init__()
        self._type = "state"
        self._name = name

class VersuInsert(utils.ParseBase):

    def __init__(self, name):
        super().__init__()
        self._type = "insert"
        self._name = name


#TODO: beliefs, quips, data


def build_parser():

    s         = pp.Suppress
    op        = pp.Optional
    lineEnd   = pp.lineEnd
    NAME      = pp.Word(pp.alphanums + "_")
    NUM       = pp.Word(pp.nums + ".")
    SEMICOLON = pp.Literal(";")
    O_BRACKET = pp.Literal('{')
    C_BRACKET = pp.Literal('}')
    O_PAR     = pp.Literal('(')
    C_PAR     = pp.Literal(')')

    FUN     = pp.Keyword('function')
    PROCESS = pp.Keyword('process')
    TYPES   = pp.Keyword('types')
    END     = pp.Keyword('end')
    START   = pp.Keyword('start')
    ACTION  = pp.Keyword('action')
    PRECON  = pp.Keyword('preconditions')
    POSTCON = pp.Keyword('postconditions')
    IF_p    = pp.Keyword('if')
    THEN    = pp.Keyword('then')
    ELSE    = pp.Keyword('else')
    CALL    = pp.Keyword('call')
    TEXT    = pp.Keyword('text')
    INSERT  = pp.Keyword('insert')
    HAND    = pp.Keyword('hand_ordered')
    DOM     = pp.Keyword('dominating')
    DEL     = pp.Keyword('delete')
    STATE   = pp.Keyword('state')

    type_p     = s(TYPES)
    process_p  = s(PROCESS) + pp.restOfLine.setResultsName('rest')
    function_p = s(FUN) + pp.restOfLine.setResultsName('rest')
    call_p     = s(CALL) + pp.restOfLine.setResultsName('rest')
    action_p   = s(ACTION) + pp.restOfLine.setResultsName('rest')
    state_p    = s(STATE) + pp.restOfLine.setResultsName('rest')
    insert_p   = s(INSERT) + pp.restOfLine.setResultsName('rest')
    end_p      = s(END)

    #result construction:
    type_p.setParseAction(lambda x: versu_e.TOPEN)
    process_p.setParseAction(lambda x: VersuProcess(x.rest))
    function_p.setParseAction(lambda x: VersuFunction(x.rest))
    call_p.setParseAction(lambda x: VersuCall(x.rest))
    action_p.setParseAction(lambda x: VersuAction(x.rest))
    state_p.setParseAction(lambda x: VersuState(x.rest))
    insert_p.setParseAction(lambda x: VersuInsert(x.rest))
    end_p.setParseAction(lambda x: versu_e.END)

    #double slash comment
    comment_line  = pp.dblSlashComment
    comment_open  = pp.Literal('/*') + pp.restOfLine
    comment_close = pp.SkipTo(pp.Literal('*/'))

    comment_line.setParseAction(lambda x: versu_e.COMMENT)
    comment_open.setParseAction(lambda x: versu_e.COPEN)
    comment_close.setParseAction(lambda x: versu_e.CCLOSE)
    comment_parser = pp.Or([pp.dblSlashComment,
                            comment_open,
                            comment_close
                            ])

    main_parser = pp.MatchFirst([comment_parser,
                                 end_p,
                                 type_p,
                                 process_p,
                                 function_p,
                                 call_p,
                                 action_p,
                                 state_p,
                                 insert_p,
                                 pp.restOfLine])

    return main_parser

def extract_from_file(filename, main_parser):
    logging.info("Extracting from: {}".format(filename))
    data = { 'comments' : 0,
             'types' : [],
             'processes' : [],
             'functions' : [],
             'calls' : [],
             'actions' : [],
             'states' : [],
             'inserts' : [] }
    lines = []
    with open(filename,'rb') as f:
        lines = [x.decode('utf-8','ignore') for x in f.readlines()]

    state = { 'bracket_count' : 0,
              'current' : None,
              'line' : 0,
              'in_block' : None }
    while bool(lines):
        state['line'] += 1
        current = lines.pop(0)

        result = main_parser.parseString(current)[0]

        if isinstance(result, utils.ParseBase):
            result._line_no = state['line']

        if result is versu_e.COPEN:
            data['comments'] += 1
            state['in_block'] = versu_e.COMMENT
        elif result is versu_e.CCLOSE:
            data['comments'] += 1
            state['in_block'] = None
        elif state['in_block'] is versu_e.COMMENT or result is versu_e.COMMENT:
            data['comments'] += 1
        elif result is versu_e.TOPEN:
            assert(state['in_block'] is None)
            state['in_block'] = versu_e.TOPEN
        elif result is versu_e.END and state['in_block'] is versu_e.TOPEN:
            state['in_block'] = None
        elif isinstance(result, VersuProcess):
            data['processes'].append(result)
        elif isinstance(result, VersuFunction):
            data['functions'].append(result)
        elif isinstance(result, VersuCall):
            data['calls'].append(result)
        elif isinstance(result, VersuAction):
            data['actions'].append(result)
        elif isinstance(result, VersuState):
            data['states'].append(result)
        elif isinstance(result, VersuInsert):
            data['inserts'].append(result)
        elif state['in_block'] is versu_e.TOPEN and result.strip() != "":
            the_type = VersuType(result.strip())
            the_type._line_no = state['line']
            data['types'].append(the_type)


    return data


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                     epilog = "\n".join([""]))
    parser.add_argument('-t', '--target')
    args = parser.parse_args()
    if args.target is not None:
        files = [args.target]
    else:
        files = utils.get_data_files([join("data","versu")], [".type", ".data", ".praxis"])

    mp = build_parser()
    for f in files:
        data = extract_from_file(f, mp)
        data_str = utils.convert_data_to_output_format(data, ['types','processes','functions','calls','actions','states','inserts'])
        utils.write_output(f, data_str, ".versu_analysis")
