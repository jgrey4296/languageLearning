"""
Get information from narrative files from data dir,

output to similarly named files in analysis directory
"""
import utils
import spacy
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
    data = { '__unique_words' : set(),
             '__total_count' : 0,
             '__sen_counts'  : {} }
    lines = []
    with open(filename,'rb') as f:
        lines = [x for x in f.read().decode('utf-8','ignore').split('\n')]

    state = { 'bracket_count' : 0,
              'current' : None,
              'line' : 0}
    while bool(lines):
        state['line'] += 1
        current = lines.pop(0)

        parsed = nlp(current)

        for sen in parsed.sents:
            for word in sen:
                if any([word.pos in [spacy.symbols.PUNCT, spacy.symbols.SPACE],
                        word.is_punct, word.is_space]):
                    continue

                word_lemma = word.lemma_.lower()
                if word_lemma not in data:
                    data['__unique_words'].add(word_lemma)
                    data[word_lemma] = 0
                data[word_lemma] += 1

            if len(sen) not in data['__sen_counts']:
                data['__sen_counts'][len(sen)] = 0
            data['__sen_counts'][len(sen)] += 1


        #possibly load dramatis personae


        #go through, find:
        ## Speech
        ## people
        ## actions


    return data


if __name__ == "__main__":
    queue = join("data", "fiction")
    input_ext = ".txt"
    output_lists = []
    output_ext = ".narrative_analysis"

    utils.standard_main(queue,
                        input_ext,
                        extract_from_file,
                        output_lists,
                        output_ext)
