"""
Script to read a convert a large  CSV file into org files
"""

from os.path import join, isfile, exists, isdir, splitext, expanduser, abspath, split
from os import listdir, mkdir
import csv
import argparse
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################



parser = argparse.ArgumentParser("")
parser.add_argument('-s', '--source')
parser.add_argument('-o', '--output')
parser.add_argument('-c', '--count', default=1000)

args = parser.parse_args()
args.source = abspath(expanduser(args.source))
args.output = abspath(expanduser(args.output))

if not exists(args.output):
    mkdir(args.output)

current_file_count = 0
current_file_size = 0

logging.info("Loading: {}".format(args.source))
with open(args.source, 'r') as f:
    csv_reader = csv.reader(f, delimiter=',')
    line_count = 0
    for row in csv_reader:
        if line_count == 0:
            line_count += 1
            continue
        line_count += 1
        categories = [x.strip() for x in row[3].split(':')]
        filtered = [x for x in categories if x != ""]
        tags = "{}:{}:".format(" " * 80, ":".join(filtered))
        with open(join(args.output, "sound_summary_{}.org".format(current_file_count)),'a') as org_file:
            org_file.write("** {}{}\n    ID: {}\n    Length: {}\n".format(row[1], tags, row[0], row[2]))
        current_file_size += 1
        if current_file_size > args.count:
            current_file_size = 0
            current_file_count += 1
            logging.info("Switching to new file: {}".format(current_file_count))
