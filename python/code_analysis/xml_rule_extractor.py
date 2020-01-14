"""
Get cif rules files from data dir,

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

    if soup.find('cifstate') is not None:
        data = extract_from_cifstate(soup)
        data['is_cifstate'] = True
    elif soup.find('promweek') is not None:
        data = extract_from_promweek(soup)
        data['is_promweek'] = True
    elif soup.find('ciflibraries') is not None:
        data = extract_from_cif_library(soup)
        data['is_cif_library'] = True


    return data

def extract_from_cif_library(soup):
    data = {}
    contents = soup.find('ciflibraries')
    data['cif_library_contents'] = list({x.name for x in contents if x.name is not None})

    data = utils.xml_search_components(data, soup, data['cif_library_contents'])


    return data

def extract_from_cifstate(soup):
    data = {}
    contents = soup.find('cifstate')
    data['cif_state_contents'] = list({x.name for x in contents if x.name is not None})

    data = utils.xml_search_components(data, soup, data['cif_state_contents'])


    return data

def extract_from_promweek(soup):
    data = { 'toplevel_components' : [] }
    contents = soup.find('promweek')
    data['prom_week_coontents'] = list({x.name for x in contents if x.name is not None})

    data = utils.xml_search_components(data, soup, data['prom_week_contents'])

    return data



if __name__ == "__main__":
    queue = join("data","xml","CiFStates")
    input_ext = ".xml"
    output_lists = []
    output_ext = ".xml_rule_analysis"

    utils.standard_main(queue,
                        input_ext,
                        extract_from_file,
                        output_lists,
                        output_ext)
