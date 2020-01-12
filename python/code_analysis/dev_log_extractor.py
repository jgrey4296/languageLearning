"""
Get DwarfFortress files from data dir,
extract names of behaviours mentioned
output to similarly named files in analysis directory
"""
import datetime
import re
import IPython
from enum import Enum
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir
from random import shuffle
import pyparsing as pp
from bs4 import BeautifulSoup
import utils

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
    data = {}
    soup = None
    with open(filename,'rb') as f:
        text = f.read().decode('utf-8', 'ignore')
        soup = BeautifulSoup(text, features='lxml')

    assert(soup is not None)
    state = { 'bracket_count' : 0,
              'current' : None,
              'line' : 0}

    if soup.find('h1') is not None:
        data = extract_from_release_info(soup)
    else:
        data = extract_from_dev_log(soup)

    return data

def extract_from_release_info(soup):
    data = {}
    title = soup.find('h1')
    release_date = soup.find('p')
    blockquote = soup.find('blockquote')

    headings = soup.find_all('h2')
    queue = headings[:]
    while bool(queue):
        current = queue.pop(0)
        if current.find_next_sibling('ul') is None:
            continue
        curr_string = current.string
        if curr_string is None:
            curr_string = current.get_text()
        curr_string = curr_string.replace("[edit]","")
        data[curr_string] = [x for x in current.find_next_sibling('ul').strings if re.match('\n',x) is None]

    data['version'] = title.string

    release_match = None
    if release_date is not None:
        release_match = re.search('was released on (.+?)\.', release_date.get_text())

    if release_match is not None:
        release_string = release_match.group(1)
    else:
        release_string = "UNKNOWN"

    data['release_date' ] = release_string

    if blockquote is not None:
        the_string = blockquote.get_text().replace('\n',' ')
        data['release_quote' ] = the_string

    return data

def extract_from_dev_log(soup):
    dev_list = soup.find('ul')

    data = {}
    try:
        for li in dev_list.children:
            span = li.find('span')
            if span is None or span == -1:
                continue
            date = span.string
            text = li.get_text()
            text = text.replace(date,"")
            text = text.replace('\n',' ')
            data[date] = text
    except AttributeError:
        breakpoint()

    return data


if __name__ == "__main__":
    queue = [join("data","dev_logs")]
    input_ext = ".html"
    output_lists = []
    output_ext = ".dev_log_analysis"

    utils.standard_main(queue,
                        input_ext,
                        extract_from_file,
                        output_lists,
                        output_ext)
