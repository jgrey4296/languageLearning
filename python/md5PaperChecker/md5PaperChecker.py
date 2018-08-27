from os.path import join, isfile, exists, isdir, splitext, expanduser, split
from os import listdir, mkdir
from hashlib import sha256
from shutil import copyfile
import IPython
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

#SOURCE_ADDED = "/Volumes/DOCUMENTS/mendeley"
#SOURCE_ADDED = "/Users/jgrey/Desktop/IPAD_MAIN"
SOURCE_ADDED = "/Users/johngrey/Documents/mendeley"

UNSORTED = [
    # "/Volumes/DOCUMENTS/Old/missingpapers",
    # "/Volumes/DOCUMENTS/Old/research",
    # "/Users/jgrey/Desktop/feb_2018_pdfs",
    # "/Users/jgrey/Desktop/feb_12_2018_pdfs"
    "/Users/johngrey/Desktop/downloadedPdfs",
    "/Users/johngrey/Desktop/papers"
    ]

DEPTHSEARCH = [
    # "/Volumes/DOCUMENTS/Papers",
    # "/Volumes/DOCUMENTS/Old",
    # "/Volumes/DOCUMENTS/mendeley"
    "/Users/johngrey/Desktop/CheckAreAdded",
    "/Users/johngrey/Desktop/pdfs_to_check"    
]
TARGET = "/Users/johngrey/Desktop/unused"

if not isdir(TARGET):
    logging.info("Making Target Dir: {}".format(TARGET))
    mkdir(TARGET)

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


#Get all Added file hashes
logging.info("Starting")
source_pdfs = getAllPdfs_deep(SOURCE_ADDED)
logging.info("Num of Source pdfs: {}".format(len(source_pdfs)))
source_hashmap = {fileToHash(x) : x for x in source_pdfs}
source_set = set(source_hashmap.keys())

#Get pdfs that haven't been added
logging.info("Checking: {}".format(UNSORTED))
other_pdfs = [y for x in UNSORTED for y in getAllPdfs(x)]
for x in DEPTHSEARCH:
    logging.info("DepthSearching: {}".format(x))
    other_pdfs += getAllPdfs_deep(x)

logging.info("Num of other pdfs: {}".format(len(other_pdfs)))
other_hashmap = {fileToHash(x) : x for x in other_pdfs}
other_set = set(other_hashmap.keys())

unused_other = other_set.difference(source_set)
logging.info("Files found: {}".format(len(unused_other)))
for x in unused_other:
    name = other_hashmap[x]
    copyfile(name, join(TARGET, split(name)[1]))


IPython.embed(simple_prompt=True)


