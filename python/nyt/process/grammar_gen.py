"""A File that loads all NYT jsons, gets the title, parses it into a POS
    graph, stores counts along the way and creates POS Sentence Chains for Top
    and Bottom 10% by likelihood nouns, and middle likelihood nouns.

"""
##############################
# IMPORTS
####################
# Setup root_logger:
import IPython
import logging as root_logger
from loader import list_data, load_file
from os.path import join
import spacy
import networkx as nx

#Log Setup
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.grammar_gen"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################
# CONSTANTS
####################
data_dir = "../../data"
grammar_dir = "../../grammars"
ROOT = "__ROOT"

nlp = spacy.load('en')

##############################
# VARIABLES
####################


##############################
# Core Functions
####################
def process_file(file):
    """ Given a file, load it, parse it,
    convert all headlines to POS tries, and save the graph
    """
    logging.info("Loading File: {}".format(file))
    data = load_file(file)
    g = nx.DiGraph()
    g.add_node('__ROOT')
    for doc in data:
        if 'headline' not in doc or 'main' not in doc['headline']:
            IPython.embed(simple_prompt=True)
            continue
        logging.info("Processing: {}".format(doc['headline']['main']))
        path = "__ROOT"
        p_text = nlp(doc['headline']['main'])
        IPython.embed(simple_prompt=True)
        for tok in p_text:
            tag = tok.tag_
            new_path = path + "." + tag
            if not g.has_edge(path, new_path):
                g.add_node(new_path, tag=tag, word_set=set())
                g.add_edge(path,new_path, weight=0)
            g[path][new_path]['weight'] += 1
            g.nodes[new_path]['word_set'].add(tok.text)

            path = new_path

        if 'keywords' not in g.nodes[path]:
            g.nodes[path]['keywords'] = []
        g.nodes[path]['keywords'] += doc['keywords']

        if '_ids' not in g.nodes[path]:
            g.nodes[path]['_ids'] = []
        g.nodes[path]['_id'] += doc['_id']

        g.nodes[path]['LEAF'] = True

    # All docs processed, save
    nx.write_gpickle(g, join(grammar_dir,file + ".pkl"))

########################################
if __name__ == "__main__":
    logging.info("Starting grammar_gen")
    files = list_data()
    process_file(files[0])
