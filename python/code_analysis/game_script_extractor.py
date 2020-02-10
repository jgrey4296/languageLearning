"""
Get setup scripts from 4x games from data dir,

output to similarly named files in analysis directory
"""
import IPython
from enum import Enum
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir
from random import shuffle
import pyparsing as pp
import utils


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

def extract_from_file(filename, main_parser):
    logging.info("Extracting from: {}".format(filename))
    data = { }
    lines = []
    with open(filename,'r') as f:
        lines = f.readlines()

    if "CK2" in filename:
        data.update(handleCK2(lines))
    elif "EUIV" in filename:
        data.update(handleEUIV(lines))
    elif "democracy_3" in filename:
        data.update(handleDemocracy(lines))
    elif "distant_worlds" in filename:
        data.update(handleDistantWorlds(lines))
    elif "geck" in filename:
        data.update(handleGECK(lines))
    elif "prison_architect" in filename:
        data.update(handlePrisonArchitect(lines))
    elif "red_shirt" in filename:
        data.update(handleRedShirt(lines))
    elif "skyrim" in filename:
        data.update(handleSkyrim(lines))
    elif "stellaris" in filename:
        data.update(handleStellaris(lines))

    return data

#Stack based, uses:
## X = { : key + context open
## } : context close
## X = Y : assignment



#democracy
#prison_architect


def handleCK2(lines):
    #CK2/EUIV scripts
    ## decision files?
    ## traits, religions!
    ##factions, titles,
    ##law, jobs, actions
    #government
    #execution methods
    #disease, death,
    #cultures
    #council voting/positions
    #buildings
    #policies
    data = {}
    state = { 'bracket_count' : 0,
              'current' : None,
              'line' : 0}
    while bool(lines):
        state['line'] += 1
        current = lines.pop(0)


    return data

def handleEUIV(lines):
    #CK2/EUIV scripts
    ## decision files?
    ## traits, religions!
    ##factions, titles,
    ##law, jobs, actions
    #government
    #execution methods
    #disease, death,
    #cultures
    #council voting/positions
    #buildings
    #policies
    data = {}
    state = { 'bracket_count' : 0,
              'current' : None,
              'line' : 0}
    while bool(lines):
        state['line'] += 1
        current = lines.pop(0)


    return data

def handleDemocracy(lines):

    data = {}
    state = { 'bracket_count' : 0,
              'current' : None,
              'line' : 0}
    while bool(lines):
        state['line'] += 1
        current = lines.pop(0)


    return data

def handleDistantWorlds(lines):
    #distant worlds
    ## policies
    data = {}
    state = { 'bracket_count' : 0,
              'current' : None,
              'line' : 0}
    while bool(lines):
        state['line'] += 1
        current = lines.pop(0)


    return data

def handleGECK(lines):

    data = {}
    state = { 'bracket_count' : 0,
              'current' : None,
              'line' : 0}
    while bool(lines):
        state['line'] += 1
        current = lines.pop(0)


    return data

def handlePrisonArchitect(lines):

    data = {}
    state = { 'bracket_count' : 0,
              'current' : None,
              'line' : 0}
    while bool(lines):
        state['line'] += 1
        current = lines.pop(0)


    return data

def handleRedShirt(lines):

    data = {}
    state = { 'bracket_count' : 0,
              'current' : None,
              'line' : 0}
    while bool(lines):
        state['line'] += 1
        current = lines.pop(0)


    return data

def handleSkyrim(lines):

    data = {}
    state = { 'bracket_count' : 0,
              'current' : None,
              'line' : 0}
    while bool(lines):
        state['line'] += 1
        current = lines.pop(0)


    return data

def handleStellaris(lines):
    #Stellaris
    ##message types
    ##policies
    ##governments
    ##event chains
    ##ethics
    ##edicts
    #diplomatic actions
    data = {}
    state = { 'bracket_count' : 0,
              'current' : None,
              'line' : 0}
    while bool(lines):
        state['line'] += 1
        current = lines.pop(0)


    return data


if __name__ == "__main__":
    queue = join("data","game_config_text")
    input_ext = ".txt"
    output_lists = []
    output_ext = ".game_analysis"

    utils.standard_main(queue,
                        input_ext,
                        extract_from_file,
                        output_lists,
                        output_ext)
