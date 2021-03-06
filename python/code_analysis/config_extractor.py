"""
Get ini files from data dir,

output to similarly named files in analysis directory
"""
import utils
import configparser
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
    config = configparser.ConfigParser(allow_no_value=True, interpolation=None)
    with open(filename, 'rb') as f:
        text = f.read().decode('utf-8','ignore')

    try:
        config.read_string(text)
        data['keys'] = config.sections()
        for section in config.sections():
            data["{}_names".format(section)] = [x[0] for x in config.items(section)]
            data["{}_values".format(section)] = [x[1] for x in config.items(section)]


    except configparser.ParsingError as e:
        logging.warning("Parse Error: {}".format(str(e)))
        data['parse_error'] = str(e)

    return data


if __name__ == "__main__":
    queue = [join("data","config_files")]
    input_ext = [".ini", ".txt"]
    output_lists = []
    output_ext = ".config_analysis"

    utils.standard_main(queue,
                        input_ext,
                        extract_from_file,
                        output_lists,
                        output_ext)
