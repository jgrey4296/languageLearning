"""
Get DwarfFortress files from data dir,
extract names of behaviours mentioned
output to similarly named files in analysis directory
"""
import IPython
from enum import Enum
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir
from random import shuffle
import pyparsing as pp
from bs4 import BeautifulSoup
import utils

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

obj_e = Enum('Parse Objects', 'ENT ACT WME BEH COM SPAWN MENTAL PRECON SPEC INIT STEP')

class DwarfFortressEnt(utils.ParseBase):

    def __init__(self, name):
        super().__init__()
        self._type = obj_e.ENT
        self._name = name

class DwarfFortressRegistration(utils.ParseBase):

    def __init__(self, _type, name):
        super().__init__()
        assert(_type in [obj_e.WME, obj_e.ACT])
        self._type = _type
        self._name = name

class DwarfFortressBehavior(utils.ParseBase):

    def __init__(self, name, args, init=False):
        super().__init__()
        self._type = obj_e.BEH
        self._init = init
        self._name = name
        self._args += args

    def add_component(self, comp):
        self._components.append(comp)

class DwarfFortressComponent(utils.ParseBase):

    def __init__(self, _type, name=None, args=None):
        super().__init__()
        assert(_type in [obj_e.SPAWN, obj_e.MENTAL, obj_e.PRECON, obj_e.SPEC, obj_e.STEP])
        self._type = _type
        self._name = name
        if args:
            self._args += args

    def __str__(self):
        _type = ""
        if self._type == obj_e.MENTAL:
            _type = "MentalAct"
        elif self._type == obj_e.PRECON:
            _type = "Precondition"
        elif self._type == obj_e.STEP:
            _type = "Step"
        elif self._type == obj_e.SPEC:
            _type = "Specificity"
        elif self._type == obj_e.SPAWN and 'subgoal' in self._args:
            _type = "SubGoal"
        elif self._type == obj_e.SPAWN and 'act' in self._args:
            _type = "Act"
        else:
            _type = "SpawnGoal"

        name = ""
        if self._name is not None:
            name = "{}".format(self._name)

        args = ", ".join([str(x) for x in self._args if x not in ["act", "spawngoal", "subgoal"]])

        return "[{}: {} ({})]".format(_type, name, args)


def build_parser():
    return
def extract_from_file(filename, DwarfFortress_parser, com_parser):
    logging.info("Extracting from: {}".format(filename))
    data = { 'behaving_entity' : "",
             'registrations' : [],
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
            result = DwarfFortress_parser.parseString(current)[0]
            #Get open and close brackets
            #handle result:
            if not result:
                continue

            result._line_no = state['line']

            if isinstance(result, DwarfFortressEnt):
                data['behaving_entity'] = result
            elif isinstance(result, DwarfFortressRegistration):
                data['registrations'].append(result)
            elif isinstance(result, DwarfFortressBehavior):
                state['current'] = result
                data['behaviors'].append(state['current'])
            elif isinstance(result, DwarfFortressComponent):
                state['current'].add_component(result)
            elif not isinstance(result, utils.ParseBase):
                logging.warning("Unrecognised parse result: {}".format(result))

    return data


if __name__ == "__main__":
    DwarfFortress_parser, com_parser = build_parser()
    files = utils.get_data_files([join("data","DwarfFortress")], ".DwarfFortress")
    for f in files:
        data = extract_from_file(f, DwarfFortress_parser, com_parser)
        data_str = utils.convert_data_to_output_format(data, ["registrations", "behaviors"])
        utils.write_output(f, data_str, ".DwarfFortress_analysis")
