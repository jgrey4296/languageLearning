"""
Get lua files from data dir,

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
# Enums:



def build_parser():

    s = pp.Suppress
    op = pp.Optional
    lineEnd = pp.lineEnd
    NAME = pp.Word(pp.alphanums + "_")
    NUM = pp.Word(pp.nums + ".")
    EQUAL = s(pp.Literal('='))
    COLON = pp.Literal(':')
    FN = s(pp.Keyword("function"))
    CLS = s(pp.Keyword('Class'))
    END = s(pp.Keyword('end'))
    SELF = pp.Keyword('self')
    LOCAL = pp.Keyword("local")
    OPAR = s(pp.Literal('('))
    CPAR = s(pp.Literal(')'))

    class_p = NAME + EQUAL + CLS + OPAR + pp.Or([FN, NAME])
    function_p = FN + NAME

    #addTask


    com_parser = None
    main_parser = None

    return main_paser, com_parser

def extract_from_file(filename, main_parser, com_parser):
    logging.info("Extracting from: {}".format(filename))
    data = {  }
    lines = []
    with open(filename,'r') as f:
        lines = f.readlines()

    state = { 'bracket_count' : 0,
              'current' : None,
              'line' : 0}
    while bool(lines):
        state['line'] += 1
        current = lines.pop(0)

        try:
            comment = com_parser.parseString(current)
            data['comments'] += 1
        except pp.ParseException:
            result = main_parser.parseString(current)
            #Get open and close brackets
            #handle result:
            if not result:
                continue

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
        files = utils.get_data_files([join("data","lua")], ".lua")

    mp, cp = build_parser()
    for f in files:
        data = extract_from_file(f, mp, cp)
        data_str = utils.convert_data_to_output_format(data, [])
        utils.write_output(f, data_str, ".lua_analysis")
