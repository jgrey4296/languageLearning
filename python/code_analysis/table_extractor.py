"""
Get abl files from data dir,
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

behavior_type_e = Enum("Behaviour Type", "SEQ PAR")
obj_e = Enum('Parse Objects', 'ENT ACT WME BEH COM SPAWN MENTAL PRECON SPEC')

def extract_from_file(filename):
    logging.info("Extracting from: {}".format(filename))
    data = { 'behaving_entity' : "",
             'acts' : [],
             'wmes' : [],
             'behaviors' : [],
             'comments' : 0
    }
    lines = []
    with open(filename,'r') as f:
        lines = f.readlines()

    state = { 'bracket_count' : 0,
              'current' : None,
              'line' : 0}
    while bool(lines):
        state['line'] += 1
        current = lines.pop(0)

        try:
            comment = com_parser.parseString(current)
            data['comments'] += 1
        except pp.ParseException:
            result = abl_parser.parseString(current)
            #Get open and close brackets
            #handle result:
            if not result:
                continue
            result_type = result[0]['type']
            result[0]['line_no'] = state['line']
            if result_type == obj_e.ENT:
                data['behaving_entity'] = result[0]
            elif result_type == obj_e.ACT:
                data['acts'].append(result[0])
            elif result_type == obj_e.WME:
                data['wmes'].append(result[0])
            elif result_type == obj_e.BEH:
                data['behaviors'].append(result[0])
            else:
                logging.warning("Unrecognised parse result: {}".format(result[0]))

    return data


if __name__ == "__main__":
    files = utils.get_data_files([join("data","tables")], ".csv")
    for f in files:
        data = extract_from_file(f)
        data_str = utils.convert_data_to_output_format(data, [])
        utils.write_output(f, data_str, ".table_analysis")
