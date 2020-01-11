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
    data['keys'] = keys
    data['length'] = len(rows)

    if 'name' in keys:
        data['names'] = [x['name'] for x in rows]

    return data


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                     epilog = "\n".join([""]))
    parser.add_argument('-t', '--target')
    args = parser.parse_args()
    if args.target is not None:
        files = [args.target]
    else:
        files = utils.get_data_files([join("data","tables")], ".csv")

    for f in files:
        data = extract_from_file(f)
        data_str = utils.convert_data_to_output_format(data, [])
        utils.write_output(f, data_str, ".table_analysis")
