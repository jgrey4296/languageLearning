from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir
from fractions import Fraction
from random import choice, shuffle
import logging as root_logger
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
        current = queue.pop()
        if current is None or current in handled:
            continue
        handled.add(current)
        sub_components = list({y.name for x in soup.find_all(current) for y in x.contents if y.name is not None})
        attrs = set([x for y in soup.find_all(current) for x in y.attrs.keys()])
        queue.update(sub_components)
        data['{}_components'.format(current)] = sub_components
        if bool(attrs):
            data['{}_attrs'.format(current)] = attrs

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

def standard_main(sources, exts, extractor, output_lists, output_ext, accumulator=None, accumulator_final=None, init_accum=None):
    import argparse
    parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                     epilog = "\n".join([""]))
    parser.add_argument('-t', '--target', action="append")
    parser.add_argument('-r', '--rand')
    parser.add_argument('-a', '--accum_name', default="accumulated_data")
    args = parser.parse_args()
    if args.target is not None:
        files = args.target
    else:
        files = get_data_files(sources, exts)

    if args.rand:
        files = [choice(files) for x in range(int(args.rand))]

    accumulated_data = init_accum
    if accumulated_data is None:
        accumulated_data = {}


    for f in files:
        data = extractor(f)
        if accumulator is not None:
            accumulated_data = accumulator(data, accumulated_data)
        data_str = convert_data_to_output_format(data, output_lists)
        write_output(f, data_str, output_ext)

    if accumulator_final is not None:
        accumulated_data = accumulator_final(accumulated_data)

    if bool(accumulated_data):
        data_str = convert_data_to_output_format(accumulated_data, output_lists)
        with open(join("analysis", args.accum_name), "w") as f:
            f.write(data_str)

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
