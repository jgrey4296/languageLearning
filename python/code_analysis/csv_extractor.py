"""
Get csv tables from data dir,
extract names of behaviours mentioned
output to similarly named files in analysis directory
"""
import utils
import csv
from enum import Enum
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir
from random import shuffle
import pyparsing as pp

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

def extract_from_file(filename):
    logging.info("Extracting from: {}".format(filename))
    data = { }

    with open(filename, 'rb') as f:
        text = [x.decode('utf-8','ignore') for x in f.readlines()]

    csv_obj = csv.DictReader(text, restkey="remaining", quotechar='"')

    rows = [x for x in csv_obj]

    keys = [x for x in rows[0].keys()]
    data['__keys'] = keys
    data['__length'] = len(rows)

    if "BBC" in filename:
        data.update(handleBBC(rows))
    elif "swda" in filename:
        data.update(handleDAMSL(rows))
    elif "democracy" in filename:
        data.update(handleDemocracy(rows))
    elif "SQF" in filename:
        data.update(handleStopAndFrisk(rows))
    elif "Badge" in filename:
        data.update(handleBadge(rows))
    else:
        logging.info("Handling Generic")
        # TODO

    return data


def handleBBC(rows):
    data = {}
    # TODO handle bbc csv
    # group, names, stats, length
    # create link to sound file : http://bbcsfx.acropolis.org.uk/assets/{wav}
    return data

def handleDAMSL(rows):
    data = {}
    # TODO handle damsl csv
    # stats on utterances
    # stats on call,response and balance of conv
    return data

def handleDemocracy(rows):
    data = {}
    # TODO handle democracy csv
    for key in ["name", "desc", "secs",
                "category", "introduce", "cancel","raise","lower",
                "department", "mincost", "maxcost",
                "cost multiplier", "implementation", "minincome"
                "maxincome", "incomemultiplier",
                "min","max", "influences", "zone",

                ]:
        if key in keys:
            data[key] = [x[key].strip() for x in rows]

    for key in ["category","zone","department"]:
        if key in keys:
            data["{}_set".format(key)] = list({x[key].strip() for x in rows})


    for row in rows:
        if "name" not in row:
            continue

        node_name = row['name'].strip()
        links = []
        values = []

        if "remaining" in row:
            cleaned = [x.strip() for x in row['remaining'] if x.strip() not in ["#",""]]
            remaining = [x.split(',') for x in cleaned if x != "#Effects"]
            links += [x[0] for x in remaining]
            try:
                values += [x[1] for x in remaining]
            except IndexError:
                logging.warning("Remain Error")
                breakpoint()

        lookup = [x for x in keys if "multiplier" in x]
        for k in lookup:
            if row[k].strip() == "":
                continue
            pairs = [x.split(",") for x in row[k].split(";")]

            try:
                links += [x[0] for x in pairs]
                values += [x[1] for x in pairs]
            except IndexError:
                logging.warning("Multiplier Error")
                breakpoint()


        if bool(links):
            data["{}_node".format(node_name)] = [('edges', links), ('equations', values)]


    return data

def handleStopAndFrisk(rows):
    data = {}
    # TODO handle stop and frisk data
    return data

def handleBadge(rows):
    data = {}
    # TODO handle badge data
    return data


if __name__ == "__main__":
    queue = [join("data","csv")]
    input_ext = ".csv"
    output_lists = []
    output_ext = ".csv_analysis"

    utils.standard_main(queue,
                        input_ext,
                        extract_from_file,
                        output_lists,
                        output_ext)
