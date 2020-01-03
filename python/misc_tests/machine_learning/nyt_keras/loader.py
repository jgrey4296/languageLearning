from os.path import join, isfile, exists, isdir, splitext, expanduser
from os import listdir
import json
import IPython
import utils
import logging as root_logger
from random import shuffle

logging = root_logger.getLogger(__name__)

def get_files(directory, filetype):
    if not isdir(directory):
        raise Exception('Not recognised as a directory :{}'.format(directory))
    listed = listdir(expanduser(directory))
    return [x for x in listed if splitext(x)[1] == filetype]

def load_nyt_json(filename):
    if not isfile(filename):
        raise Exception("Not recognised as a file: {}".format(filename))
    logging.info('Loading: {}'.format(filename))
    with open(filename) as f:
        data = json.load(f)
    if 'response' not in data:
        raise Exception('Data is lacking a response field')
    return data['response']['docs']


def get_title_and_paragraph(dir, num=None, rand=False):
    """ Get a 2d list of pairs. Each pair holds the title, and a paragraph, from
    a news article. Num specifies the 1st dimension, the number of
    files/months to retrieve from """
    files = get_files(dir, '.json')
    if num is None:
        num = 1
    if not rand:
        x = files[num]
    else:
        shuffle(files)
        x = files.pop()
    docs = load_nyt_json(join(dir,x))
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
    return pairs
