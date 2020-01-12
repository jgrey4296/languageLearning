"""
Get xml files of text (usc, the bible etc) from data dir,

output to similarly named files in analysis directory
"""

from enum import Enum
from os import listdir
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from random import shuffle
import csv
import pyparsing as pp
import re
import spacy
import utils

#Choose:
#https://docs.python.org/3/library/xml.etree.elementtree.html
import lxml
from bs4 import BeautifulSoup


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

def extract_from_file(filename):
    logging.info("Extracting from: {}".format(filename))
    data = { }

    with open(filename, 'rb') as f:
        text = f.read().decode('utf-8','ignore')

    soup = BeautifulSoup(text, features='lxml')


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
        base = ["data", "xml"]
        queue = [join(*base, x) for x in ["uscode",
                                          "king_james_bible",
                                          "red_shirt",
                                          "king_dragon_pass",
                                          "unrest",
                                          "twine"]]
        files = utils.get_data_files(queue, ".xml")

    for f in files:
        data = extract_from_file(f)
        data_str = utils.convert_data_to_output_format(data, [])
        utils.write_output(f, data_str, ".xml_text_analysis")
