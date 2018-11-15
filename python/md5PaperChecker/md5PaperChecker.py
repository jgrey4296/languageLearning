#------------------------------
# Simple program to integrate papers into a collection   
#------------------------------

from os.path import join, isfile, exists, isdir, splitext, expanduser, split
from os import listdir, mkdir
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

#The destination where papers already exist
SOURCE_ADDED = "/Volumes/DOCUMENTS/mendeley"
#SOURCE_ADDED = "/Users/jgrey/Desktop/IPAD_MAIN"
#SOURCE_ADDED = None

#The source of potentially un-integrated papers
UNSORTED = [
    ]
    #"/Volumes/DOCUMENTS/Old/missingpapers",
    # "/Volumes/DOCUMENTS/Old/research",
    # "/Users/jgrey/Desktop/feb_2018_pdfs",
    # "/Users/jgrey/Desktop/feb_12_2018_pdfs"
    # ]
#directories to depth search for papers
DEPTHSEARCH = [
    #"~/mega/pdfs"
     "/Volumes/DOCUMENTS/Papers",
     "/Volumes/DOCUMENTS/Old",
    # "/Volumes/DOCUMENTS/mendeley"
]
#Where to put papers that need to be integrated
TARGET = "/Users/jgrey/Desktop/deduplicated"

if not isdir(TARGET):
    logging.info("Making Target Dir: {}".format(TARGET))
    mkdir(expanduser(TARGET))

def getAllPdfs_deep(loc):
    """ Get the full paths of all pdfs in the location """
    loc = expanduser(loc)
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
    loc = expanduser(loc)
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


#Get all Added file hashes
logging.info("Starting")
source_pdfs = []
if SOURCE_ADDED is not None:
    source_pdfs = getAllPdfs_deep(SOURCE_ADDED)
logging.info("Num of Source pdfs: {}".format(len(source_pdfs)))
source_hashmap = {}
for x in source_pdfs:
    file_hash = fileToHash(x)
    if file_hash in source_hashmap:
        logging.warning("Conflict: {} - {} - {}".format(file_hash, x, source_hashmap[file_hash]))
    else:
        source_hashmap[file_hash] = x
source_set = set(source_hashmap.keys())

other_pdfs = [y for x in UNSORTED for y in getAllPdfs(x)]
for x in DEPTHSEARCH:
    other_pdfs += getAllPdfs_deep(x)

logging.info("Num of other pdfs: {}".format(len(other_pdfs)))
other_hashmap = {}
for x in other_pdfs:
    file_hash = fileToHash(x)
    if file_hash in other_hashmap:
        logging.warning("Conflict: {} - {} - {}".format(file_hash, x, other_hashmap[file_hash]))
    else:
        other_hashmap[file_hash] = x
other_set = set(other_hashmap.keys())

unused_other = other_set.difference(source_set)
logging.info("Files found: {}".format(len(unused_other)))
for x in unused_other:
    name = other_hashmap[x]
    copyfile(name, join(TARGET, split(name)[1]))


IPython.embed(simple_prompt=True)


