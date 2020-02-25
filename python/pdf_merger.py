#adapted from https://www.binpress.com/pdfrw-python-pdf-library/
#use "pip install pdfrw" first
#assumes python3
from pdfrw import PdfReader, PdfWriter, PageMerge
import argparse
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir
import IPython
#see https://docs.python.org/3/howto/argparse.html
parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                 epilog = "\n".join([""]))
parser.add_argument('-f', '--first')
parser.add_argument('-s', '--second')
parser.add_argument('-o', '--out')
args = parser.parse_args()

args.first = abspath(expanduser(args.first))
args.second = abspath(expanduser(args.second))
args.out = abspath(expanduser(args.out))

even = PdfReader(args.first)
odd = PdfReader(args.second)
all = PdfWriter()
for x in range(max(len(even.pages), len(odd.pages))):
    print("Adding Page: {}".format(x))
    if x < len(even.pages):
        print("Even")
        all.addpage(even.pages[x])
    if x < len(odd.pages):
        print("Odd")
        all.addpage(odd.pages[x])

all.write(args.out)
