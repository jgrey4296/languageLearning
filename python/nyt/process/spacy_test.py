"""
    Loading some nyt data, parsing it, and replacing words in it
"""
# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.spacy_test"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################
# IMPORTS
####################
import spacy
import en_core_web_sm
from spacy.symbols import VERB
en = en_core_web_sm.load()
import json
from os.path import join, isfile, exists, isdir, splitext, expanduser
from os import listdir
from numpy.random import choice
import IPython

##############################
# CONSTANTS
####################
DATA_LOC = 'data'
NUM_OF_FILES_TO_LOAD = 3
SEMI = en(';')[0].lex_id
##############################
# VARIABLES
####################


##############################
# Utilities
####################

##############################
# Core Functions
####################
def load_data(num):
    """ Load a number of pieces of data from a random nyt response  """
    loaded_data = []

    files = listdir(DATA_LOC)
    chosen = choice(files, num)
    paths = [join(DATA_LOC,x) for x in chosen]

    for path in paths:
        logging.info('Loading: {}'.format(path))
        with open(path,'r') as f:
            temp_data = json.load(f)
            loaded_data.append(temp_data['response']['docs'])

    return loaded_data

def mod_data(data):
    """ Data: a single result from a loaded json,
    modify it to have additional fields, namely:
    parsed_headline : the spacy parse tree of the headline.main field
    """
    if not ('headline' in data and 'main' in data['headline']):
        return None
    headline = data['headline']['main']
    headline_p = en(headline)
    copied = data.copy()
    copied['headline_p'] = headline_p
    return copied

def prepare_docs(all_loaded):
    """ all_loaded : [json_loaded_from_files] """
    prepared = []
    for i,doc in enumerate(all_loaded):
        logging.info("Preparing doc: {}".format(i))
        modded = [mod_data(x) for x in doc]
        prepared.append(modded)
    return prepared


def has_verb_predicate(doc):
    """ Test the doc to see if it has a verb in it """
    parts_of_speech = set([x.pos for x in doc['headline_p']])
    if VERB in parts_of_speech:
        return True
    else:
        return False

def get_highest_verb(doc):
    """ Get the verb closest to the root of the sentence """
    verbs = [x for x in doc['headline_p'] if x.pos == VERB]
    depth_pairs = [(x, count_steps_to_self_head(x)) for x in verbs]
    return min(depth_pairs, key=lambda pair: pair[1])


def count_steps_to_self_head(tok):
    """ Given a token, get its distance from the root of the sentence """
    count = 0
    cur_tok = tok
    while cur_tok.head != cur_tok:
        count += 1
        cur_tok = cur_tok.head
    return count

def get_headline_verb_pairs(doc):
    """ Pair the headline of a doc with its primary verb """
    return (doc['headline_p'], get_highest_verb(doc))

def split_on_semicolon(doc):
    """ Given a doc, add separated phrases to it based on its headline """
    parsed = doc['headline_p']
    num_of_roots = len([x for x in doc['headline_p'] if x.dep_ == 'ROOT'])
    semicolons = [x for x in doc['headline_p'] if x.lex_id == SEMI]
    has_semicolon = len(semicolons)
    if num_of_roots == 1:
        doc['phrases'] = doc['headline_p']
    elif has_semicolon:
        doc['phrases'] = [parsed[0:semicolons[0].i], parsed[semicolons[0].i+1:]]
    else:
        logging.warning("Confused on split semicolon")
        logging.warning(doc['headline_p'])
        logging.warning([(x,x.dep_, x.dep) for x in doc['headline_p']])
        logging.warning('-----')
        doc['phrases'] = []
    return doc

########################################
if __name__ == "__main__":
    logging.info("Starting ")
    all_loaded_data = load_data(NUM_OF_FILES_TO_LOAD)
    prepared = prepare_docs(all_loaded_data)
    filtered_out_nones = [x for x in prepared if x is not None]
    flattened = [x for doc in filtered_out_nones for x in doc]
    verb_filtered = [x for x in flattened if has_verb_predicate(x)]
    headline_verb_pairs = [get_headline_verb_pairs(x) for x in verb_filtered]
    nouns = [list(x['headline_p'].noun_chunks) for x in flattened]

    IPython.embed(simple_prompt=True)
