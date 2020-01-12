from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir
from random import choice
import logging as root_logger
from random import shuffle
logging = root_logger.getLogger(__name__)

class ParseBase:

    def __init__(self):
        self._type = None
        self._name = None
        self._components = []
        self._args = []
        self._line_no = -1

    def __str__(self):
        arg_s = ""
        comp_s = ""

        s = "{} : {} : {}{}{}"

        if bool(self._args):
            arg_s = " : {}".format(", ".join([str(x) for x in self._args]))

        if bool(self._components):
            comp_s = " := {}".format(", ".join([str(x) for x in self._components]))

        return s.format(self._line_no,
                        self._type,
                        self._name,
                        arg_s,
                        comp_s)



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

def convert_data_to_output_format(data, loop_on_keys=None):
    logging.info("Converting to output format")
    #expect a dictionary for data
    output_str = ""
    if not loop_on_keys:
        loop_on_keys = []
    for k,v in data.items():
        if k in loop_on_keys:
            for item in v:
                output_str += "{}\n".format(str(item))
        else:
            output_str += "{} : {}\n".format(k,str(v))

    return output_str

def write_output(source_path, data_str, ext):
    logging.info("Writing output to analysis file")
    src_name = splitext(split(source_path)[1])[0]
    header = split(split(source_path)[0])[1]
    analysis_name = "{}_{}{}".format(header,src_name, ext)
    analysis_path = join("analysis",analysis_name)

    if exists(analysis_path):
        logging.warning("Analysis path already exists: {}".format(analysis_path))
        tmp = list("abcdefg")
        shuffle(tmp)
        analysis_name = "{}_{}_{}{}".format(header,src_name,"".join(tmp), ext)
        analysis_path = join("analysis",analysis_name)

    with open(analysis_path,'w') as f:
        f.write(data_str)

def standard_main(sources, exts, extractor, output_lists, output_ext):
    import argparse
    parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                     epilog = "\n".join([""]))
    parser.add_argument('-t', '--target')
    parser.add_argument('-r', '--rand')
    args = parser.parse_args()
    if args.target is not None:
        files = [args.target]
    else:
        files = get_data_files(sources, exts)

    if args.rand:
        files = [choice(files) for x in range(int(args.rand))]

    for f in files:
        data = extractor(f)
        data_str = convert_data_to_output_format(data, output_lists)
        write_output(f, data_str, output_ext)
