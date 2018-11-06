"""
    A File to check that all filenames exist in the target directory
"""
##############################
# IMPORTS
####################
# Setup root_logger:
import IPython
import logging as root_logger
from os.path import join, isfile, exists, isdir, splitext, expanduser
from os import listdir

#Log Setup
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.checkAllExist"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################
# CONSTANTS
####################
HTML = ".html"
TARGET_DIR = "~/Mega/savedTwitter"
SOURCE_DIRS = [
    "~/Desktop/savedTwitter",
    "~/Desktop/new_saved_twitter"
]

def collect_sources():
    collected = set()
    for x in SOURCE_DIRS:
        contents = listdir(expanduser(x))
        html_files = [a for a in contents if splitext(a)[1] == HTML]
        collected.update(html_files)    
    return collected

def collect_target():
    collected = set()
    contents = listdir(expanduser(TARGET_DIR))
    html_files = [a for a in contents if splitext(a)[1] == HTML]
    collected.update(html_files)
    return collected

########################################
if __name__ == "__main__":
    logging.info("Starting ")
    sources = collect_sources()
    target = collect_target()

    diff = sources.difference(target)
    if bool(diff):
        logging.warn("There was a difference")
        logging.warn(diff)
    else:
        logging.info("All Good")
    IPython.embed(simple_prompt=True)
