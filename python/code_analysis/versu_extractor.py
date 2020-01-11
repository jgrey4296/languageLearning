"""
Get versu files from data dir,

output to similarly named files in analysis directory
"""
import IPython
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


def build_parser():

    s = pp.Suppress
    op = pp.Optional
    lineEnd = pp.lineEnd
    NAME = pp.Word(pp.alphanums + "_")
    NUM = pp.Word(pp.nums + ".")
    SEMICOLON = pp.Literal(";")
    O_BRACKET = pp.Literal('{')
    C_BRACKET = pp.Literal('}')
    O_PAR = pp.Literal('(')
    C_PAR = pp.Literal(')')

    FUN = pp.Keyword('function')
    PROCESS = pp.Keyword('process')
    TYPES = pp.Keyword('types')
    END = pp.Keyword('end')
    START = pp.Keyword('start')
    ACTION = pp.Keyword('action')
    PRECON = pp.Keyword('preconditions')
    POSTCON = pp.Keyword('postconditions')
    IF_p = pp.Keyword('if')
    THEN = pp.Keyword('then')
    ELSE = pp.Keyword('else')
    CALL = pp.Keyword('call')
    TEXT = pp.Keyword('text')
    INSERT = pp.Keyword('insert')
    HAND = pp.Keyword('hand_ordered')
    DOM = pp.Keyword('dominating')
    DEL = pp.Keyword('delete')


    #double slash comment

    main_parser = None
    comment_parser = None

    return main_parser, comment_parser

def extract_from_file(filename, main_parser, comment_parser):
    logging.info("Extracting from: {}".format(filename))
    data = {}
    lines = []
    with open(filename,'r') as f:
        lines = f.readlines()

    state = { 'bracket_count' : 0,
              'current' : None,
              'line' : 0}
    while bool(lines):
        state['line'] += 1
        current = lines.pop(0)

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
        files = utils.get_data_files([join("data","versu")], [".type", ".data", "praxis"])

    mp, cp = build_parser()
    for f in files:
        data = extract_from_file(f, mp, cp)
        data_str = utils.convert_data_to_output_format(data, [])
        utils.write_output(f, data_str, ".versus_analysis")
