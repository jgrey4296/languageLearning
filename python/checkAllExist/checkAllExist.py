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
from os import listdir, mkdir
from subprocess import call

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
SOURCE_DIR = "/Volumes/DOCUMENTS/mac mini/mac_mini_savedTwitter"

RESULT_DIR = "~/Desktop/missing_twitter/"

def collect(x):
    collected = set()
    contents = listdir(expanduser(x))
    html_files = [a for a in contents if splitext(a)[1] == HTML]
    collected.update(html_files)    
    return collected

def copy(files):
    assert(isinstance(files, set))
    if not exists(expanduser(RESULT_DIR)):
        mkdir(expanduser(RESULT_DIR))
    
    for x in files:
        logging.info("To Copy: {}".format(x))
        #copy the html
        call(['cp', join(SOURCE_DIR,x), expanduser(RESULT_DIR)])
        #copy the files
        f_name, f_type = splitext(x)
        call(['cp', '-r', join(SOURCE_DIR, (f_name + "_files")), expanduser(RESULT_DIR)])
        

########################################
if __name__ == "__main__":
    logging.info("Starting ")
    sources = collect(SOURCE_DIR)
    target = collect(TARGET_DIR)

    diff = sources.difference(target)
    copy(diff)
    
    if bool(diff):
        logging.warn("There was a difference")
        logging.warn(diff)

    else:
        logging.info("All Good")
    IPython.embed(simple_prompt=True)
