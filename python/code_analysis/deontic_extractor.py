"""
Get rules from text in data dir,

output to similarly named files in analysis directory
"""
import utils
import IPython
from enum import Enum
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir
from random import shuffle
import pyparsing as pp

nlp = spacy.load("en_core_web_sm")

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
    return None

def extract_from_file(filename):
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
    queue = join("data","natural_langauge_deontics")
    input_ext = ".txt"
    output_lists = []
    output_ext = ".deontic_analysis"

    utils.standard_main(queue,
                        input_ext,
                        extract_from_file,
                        output_lists,
                        output_ext)
