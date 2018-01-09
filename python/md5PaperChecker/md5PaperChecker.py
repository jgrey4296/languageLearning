from os.path import join, isfile, exists, isdir, splitext, expanduser, split
from os import listdir
from hashlib import sha256
from shutil import copyfile
import IPython
#https://ipython.readthedocs.io/en/stable/config/options/terminal.html
#IPython.embed(simple_prompt=True)
#in shell: ipython --simple-prompty --matplotlib
# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.md5PaperChecker"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################

MENDELEY = "/Volumes/DOCUMENTS/mendeley"
UNSORTED = [
    "/Volumes/DOCUMENTS/Old/missingpapers",
    "/Volumes/DOCUMENTS/Old/research",
    ]

DEPTHSEARCH = "/Volumes/DOCUMENTS/Papers"
TARGET = "/Users/jgrey/Desktop/unused"


def getAllPdfs_deep(loc):
    """ Get the full paths of all pdfs in the location """
    assert(exists(loc))
    assert(isdir(loc))
    queue = [loc]
    found = []
    while bool(queue):
        l = queue.pop()
        entries = listdir(l)
        pdfs = [join(l,x) for x in entries if splitext(x)[1] == ".pdf"]
        dirs = [join(l,x) for x in entries if isdir(join(l,x))]
        queue += dirs
        found += pdfs
    return found

def getAllPdfs(loc):
    """ Get all the pdfs in a directory """
    assert(exists(loc))
    assert(isdir(loc))
    found = []
    for x in listdir(loc):
        if splitext(x)[1] == ".pdf":
            found.append(join(loc,x))
    return found

def fileToHash(filename):
    try:
        assert(isfile(filename))
    except:
        IPython.embed(simple_prompt=True)
    with open(filename, 'rb') as f:
        return sha256(f.read()).hexdigest()


#Get all mendeley file hashes
logging.info("Starting")
mendeley_pdfs = getAllPdfs_deep(MENDELEY)
logging.info("Num of Mendeley pdfs: {}".format(len(mendeley_pdfs)))
mendeley_hashmap = {fileToHash(x) : x for x in mendeley_pdfs}
mendeley_set = set(mendeley_hashmap.keys())

other_pdfs = [y for x in UNSORTED for y in getAllPdfs(x)]
other_pdfs += getAllPdfs_deep(DEPTHSEARCH)

logging.info("Num of other pdfs: {}".format(len(other_pdfs)))
other_hashmap = {fileToHash(x) : x for x in other_pdfs}
other_set = set(other_hashmap.keys())

unused_other = other_set.difference(mendeley_set)
logging.info("Files found: {}".format(len(unused_other)))
for x in unused_other:
    name = other_hashmap[x]
    copyfile(name, join(TARGET, split(name)[1]))


IPython.embed(simple_prompt=True)


