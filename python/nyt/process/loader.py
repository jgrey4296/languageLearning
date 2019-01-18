"""
Code to load NYT response jsons
"""
from os.path import join, isfile, exists, isdir, splitext, expanduser
from os import listdir
import json
import IPython

import logging as root_logger
logging = root_logger.getLogger(__name__)

##############################
DATA_DIR = "../../data"
DATA_TYPE = ".json"

def list_data(dir=DATA_DIR, filetype=DATA_TYPE):
    """ Get the json files in a directory """
    if not isdir(dir):
        raise Exception('Not recognised as a directory: {}'.format(dir))
    files = listdir(dir)
    files_of_type = [splitext(x)[0] for x in files if splitext(x)[1] == filetype]
    return files_of_type


def load_file(filename, dir=DATA_DIR, filetype=DATA_TYPE):
    """ load a nyt response json file and return the docs in the file  """
    file_loc = join(dir,(filename + filetype))
    if not isfile(file_loc):
        raise Exception("Not recognised as a file: {}".format(file_loc))
    logging.info('Loading: {}'.format(file_loc))
    with open(file_loc) as f:
        data = json.load(f)
    if 'response' not in data:
        raise Exception('Data is lacking a response field')
    return data['response']['docs']

def get_title_and_paragraph(dir, num=None):
    """ Get a 2d list of pairs. Each pair holds the title, and a paragraph, from
    a news article. Num specifies the 1st dimension, the number of
    files/months to retrieve from """
    file_data = []
    files = list_data(dir)
    if num is None:
        num = len(files)
    for x in files[:num]:
        docs = load_file(join(dir,x))
        pairs = []
        for doc in docs:
            if 'main' in doc['headline'] and doc['headline']['main'] is not None:
                title = doc['headline']['main']
            else:
                title = ''
            if 'print_headline' in doc['headline'] and doc['headline']['print_headline'] is not None:
                alt_title = doc['headline']['print_headline']
            else:
                alt_title = ''

            if 'abstract' in doc and doc['abstract'] is not None: 
                abstract = doc['abstract']
            else:
                abstract = ''
            if 'lead_paragraph' in doc and doc['lead_paragraph'] is not None:
                lead = doc['lead_paragraph']
            else:
                lead = ''
            if 'snippet' in doc and doc['snippet'] is not None:
                snippet = doc['snippet']
            else:
                snippet = ''

            chosen_title = max((title, len(title)),
                               (alt_title, len(alt_title)),
                               key=lambda x: x[1])[0]
            chosen_text = max((abstract, len(abstract)),
                              (lead, len(lead)),
                              (snippet, len(snippet)),
                              key=lambda x: x[1])[0]

            if len(chosen_title) > 0 and len(chosen_text) > 0:
                pairs.append((chosen_title, chosen_text))
        file_data.append(pairs)
    return file_data

