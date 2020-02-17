import argparse
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir
from time import sleep
from fractions import Fraction
from random import choice, shuffle
import logging as root_logger
import requests
logging = root_logger.getLogger(__name__)

class ParseBase:

    def __init__(self):
        self._type = None
        self._name = None
        self._components = []
        self._args = []
        self._line_no = -1

    def __repr__(self):
        return "({} : {} : {})".format(self._line_no,
                                       self._type,
                                       self._name)

    def __str__(self):
        arg_s = ""
        comp_s = ""

        if bool(self._args):
            arg_s = " : {}".format(", ".join([str(x) for x in self._args]))

        if bool(self._components):
            comp_s = " := {}".format(" ".join([str(x) for x in self._components]))

        s = "{} : {} : {}{}{}"

        return s.format(self._line_no,
                        self._type,
                        self._name,
                        arg_s,
                        comp_s)

    def __lt__(self, other):
        return self._line_no < other._line_no


class Trie:

    def __init__(self, value, path, example=None):
        self.count = 0
        self.value = value
        self.path = "{} {}".format(path, value)
        self.data = {}
        self._example = example

    def __repr__(self):
        #Print all paths to leaves
        return "\n".join(self.leaves())

    def __bool__(self):
        """ Is Node populated? """
        return bool(self.data)

    def get(self, key):
        if key not in self.data:
            self.data[key] = Trie(key, self.path)

        return self.data[key]

    def inc(self):
        self.count += 1

    def add_string(self, theList, transform=None, example=None):
        if transform is None:
            transform = lambda x: x
        current = self
        for val in theList:
            val_prime = transform(val)
            current = current.get(val_prime)
            current.inc()
        if example and current._example is None:
            current._example = example

    def leaves(self):
        leaves = []
        queue = [self]
        while queue:
            node = queue.pop(0)
            if not node:
                leaves.append("{} :: {} / {} : {}".format(node.path, node.count.numerator, node.count.denominator, node._example))
            else:
                queue += list(node.data.values())

        return leaves

    def convert_to_rational(self, total_count):
        if not isinstance(self.count, Fraction):
            self.count = Fraction(self.count, total_count)
        total_count = sum([x.count for x in self.data.values()])
        for x in self.data.values():
            x.convert_to_rational(total_count)


    def construct_likely_path(self):
        path = ""
        current = self
        while bool(self):
            children = list(self.data.values())
            prob_pairs = [(x.count, x) for x in children]

            #random selection
            current = prob_pairs[0][1]
            path += " {} ({}) ".format(current.key, str(current.count))

        return path


def xml_search_components(data, soup, initial):
    """ Summarize a file's tags and attributes, hierarchically """
    queue = set(initial)
    handled = set()
    while bool(queue):
        logging.info("Queue len: {}".format(len(queue)))
        current = queue.pop()
        if current is None or current in handled:
            continue
        handled.add(current)
        sub_components = list({y.name for x in soup.find_all(current) for y in x.contents if y.name is not None})
        attrs = set([x for y in soup.find_all(current) for x in y.attrs.keys()])
        queue.update(sub_components)
        data['components_{}'.format(current)] = sub_components
        if bool(attrs):
            data['attrs_{}'.format(current)] = attrs

    return data

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

def source_path_to_output_path(source_path, ext):
    src_name = splitext(split(source_path)[1])[0]
    header = split(split(source_path)[0])[1]
    analysis_name = "{}_{}{}".format(header,src_name, ext)
    orig_analysis_path = join("analysis",analysis_name)
    unique_analysis_path = orig_analysis_path

    if exists(orig_analysis_path):
        logging.warning("Analysis path already exists: {}".format(orig_analysis_path))
        tmp = list("abcdefg")
        shuffle(tmp)
        analysis_name = "{}_{}_{}{}".format(header,src_name,"".join(tmp), ext)
        unique_analysis_path = join("analysis",analysis_name)

    return unique_analysis_path, orig_analysis_path


def write_output(source_path, data_str, ext):
    logging.info("Writing output to analysis file")
    u_analysis_path, other = source_path_to_output_path(source_path, ext)
    with open(u_analysis_path,'w') as f:
        f.write(data_str)

def standard_main(sources, exts, extractor, output_lists, output_ext, accumulator=None, accumulator_final=None, init_accum=None):
    """ Standardised main function for extractors.
    Handles parser arguments, finds files that match extensions in the source directories specified,
    parses found files using the provided extractor,
    prints out data, expanded specified output lists, into a filename with output_ext.

    Can accumulate data across files, and then apply a final function to that accumulation"""

    parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                     epilog = "\n".join([""]))
    parser.add_argument('-t', '--target', action="append")
    parser.add_argument('-r', '--randn')
    parser.add_argument('--randarg', action="store_true")
    parser.add_argument('--filter', action="store_true")
    parser.add_argument('-a', '--accum_name', default="accumulated_data")
    args = parser.parse_args()



    # Get files, or use CLI specified targets
    if args.target is not None:
        files = get_data_files(args.target, exts)
    else:
        files = get_data_files(sources, exts)

    # Choose subselection of files if necessary
    if args.randn:
        files = [choice(files) for x in range(int(args.randn))]
    elif args.randarg:
        shuffle(files)

    #filter already processed
    if args.filter:
        filtered_files = []
        for x in files:
            u_path, o_path = source_path_to_output_path(x, output_ext)
            if exists(o_path):
                continue
            filtered_files.append(x)
            files = filtered_files

    # Initialise accumulation data
    accumulated_data = init_accum
    if accumulated_data is None:
        accumulated_data = {}

    # Process each found file:
    for f in files:
        # Extract data:
        data = extractor(f)
        if accumulator is not None:
            accumulated_data = accumulator(data, accumulated_data)
        # Convert to string
        data_str = convert_data_to_output_format(data, output_lists)
        # Write it out
        write_output(f, data_str, output_ext)

    # Apply final accumulator function
    if accumulator_final is not None:
        accumulated_data = accumulator_final(accumulated_data)

    # Write out final accumulation
    if bool(accumulated_data):
        data_str = convert_data_to_output_format(accumulated_data, output_lists)
        with open(join("analysis", args.accum_name), "w") as f:
            f.write(data_str)



CONCEPT_NET_API = "http://api.conceptnet.io{}"
def search_conceptnet(concept):
    """ Rate Limit: 3600 an hour, 120 a minute.
    Average at 1 per second
    Expect concept to start with a /
    """
    assert(concept[0] == "/")
    sleep(1)
    return requests.get(CONCEPT_NET_API.format(concept)).json()


def map_text(text):
    """ Given some text, create a mapping to integers and back """
    #todo: enable it to work for tokens as well
    chars = sorted(list(set(text)))
    char_indices = dict((c, i) for i, c in enumerate(chars))
    indices_char = dict((i, c) for i, c in enumerate(chars))
    return (char_indices, indices_char)

def sample(predictions, temperature=1.0):
    """ For a word mapping M:{i : char} dictionary, give [] of len(M) of predictions of
    the next index. Normalize it and sample, taking the highest. Return that index """
    #cast to high precision?
    preds = np.asarray(predictions).astype('float64')
    preds = np.log(pres) / temperature
    exp_preds = np.exp(preds)
    #normalize
    preds = exp_preds / np.sum(exp_preds)
    probabilities = np.random.multinomial(1, preds, 1)
    return np.argmax(probas)
