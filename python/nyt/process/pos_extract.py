from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir
import json
import argparse
import spacy

nlp = spacy.load("en_core_web_sm")

the_trie = {}

#see https://docs.python.org/3/howto/argparse.html
parser = argparse.ArgumentParser("")
parser.add_argument('-f', '--file')

args = parser.parse_args()
args.file = abspath(expanduser(args.file))


#r/w/rb/wb
with open(args.file, 'r') as f:
     data = json.load(f)

#for each document in data['response']['docs']
# get the headline, parse get tag_, then add to trie
for d in data['response']['docs']:
    if 'headline' in d and 'main' in d['headline']:
        doc = nlp(d['headline']['main'])
        the_trie.add([tok._tag for tok in doc])


# after:
# save to reloadable format
the_trie.export()
