"""
Split multi-page pdfs into separate files
"""
#adapted from https://www.binpress.com/pdfrw-python-pdf-library/
#use "pip install pdfrw" first
#assumes python3
from pdfrw import PdfReader, PdfWriter, PageMerge
import argparse
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir, mkdir
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

#see https://docs.python.org/3/howto/argparse.html
parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                 epilog = "\n".join([""]))
parser.add_argument('-d', '--directory')
parser.add_argument('-o', '--out')
args = parser.parse_args()

args.directory= abspath(expanduser(args.directory))
args.out= abspath(expanduser(args.out))

if not exists(args.out):
    mkdir(args.out)

def get_data_files(initial, ext):
    logging.info("Getting Data Files")
    if not isinstance(ext, list):
        ext = [ext]
    if not isinstance(initial, list):
        initial = [initial]
    files = []
    queue = initial[:]
    while bool(queue):
        current = queue.pop(0)
        if isfile(current) and splitext(current)[1] in ext:
            files.append(current)
        elif isdir(current):
            sub = [join(current,x) for x in listdir(current)]
            queue += sub

    logging.info("Found {} {} files".format(len(files), ext))
    return files

pdfs = get_data_files(args.directory, '.pdf')

logging.info("Chopping pdfs")
for pdf in pdfs:
    logging.info("Reading: {}".format(pdf))
    data = PdfReader(pdf)
    edited = PdfWriter()

    for x in range(1, len(data.pages)):
        edited.addpage(data.pages[x])

    out_name = join(args.out, split(pdf)[1])
    logging.info("Writing to: {}".format(out_name))
    edited.write(out_name)
    logging.info("-----------")
