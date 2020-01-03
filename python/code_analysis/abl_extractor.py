"""
Get abl files from data dir,
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

class AblEnt(utils.ParseBase):

    def __init__(self, name):
        super().__init__()
        self._type = obj_e.ENT
        self._name = name

class AblRegistration(utils.ParseBase):

    def __init__(self, _type, name):
        super().__init__()
        assert(_type in [obj_e.WME, obj_e.ACT])
        self._type = _type
        self._name = name

class AblBehavior(utils.ParseBase):

    def __init__(self, name, args, init=False):
        super().__init__()
        self._type = obj_e.BEH
        self._init = init
        self._name = name
        self._args += args

    def add_component(self, comp):
        self._components.append(comp)

class AblComponent(utils.ParseBase):

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

    s = pp.Suppress
    op = pp.Optional
    lineEnd = pp.lineEnd
    NAME = pp.Word(pp.alphanums + "_")
    NUM = pp.Word(pp.nums + ".")
    SEMICOLON = pp.Literal(";")
    O_BRACKET = pp.Literal('{')
    C_BRACKET = pp.Literal('}')

    act_abl = pp.Keyword("act")
    atomic_abl = pp.Keyword("atomic")
    be_abl = pp.Keyword("behaving_entity")
    behavior_abl = pp.Keyword("behavior")
    fail_abl = pp.Keyword("fail_step")
    joint_abl = pp.Keyword("joint")
    mental_abl = pp.Keyword("mental_act")
    parallel_abl = pp.Keyword("parallel")
    precond_abl = pp.Keyword("precondition")
    register_abl = pp.Keyword("register")
    sequential_abl = pp.Keyword("sequential")
    spawn_abl = pp.Keyword("spawngoal")
    specificity_abl = pp.Keyword("specificity")
    subgoal_abl = pp.Keyword("subgoal")
    with_abl = pp.Keyword("with")
    wme_abl = pp.Keyword("wme")
    initial_abl = pp.Keyword("initial_tree")
    succeed_abl = pp.Keyword("succeed_step")
    fail_abl = pp.Keyword("fail_step")


    parallel_abl.setParseAction(lambda x: "Parallel")
    sequential_abl.setParseAction(lambda x: "Sequential")

    beh_ent_stmt = s(be_abl) + NAME
    register_act_stmt = s(register_abl + act_abl) + NAME
    register_wme_stmt = s(register_abl + wme_abl) + NAME

    behavior_stmt = (op(atomic_abl) + op(joint_abl)).setResultsName("args") + pp.Or([sequential_abl, parallel_abl]).setResultsName('form') + s(behavior_abl) + pp.Group(NAME).setResultsName("name")

    spawn_stmt = pp.Or([spawn_abl, subgoal_abl, act_abl]) + NAME
    skip_to_spawn = s(pp.SkipTo(spawn_stmt)) + spawn_stmt
    step_stmt = pp.Or([succeed_abl, fail_abl])
    skip_to_step = s(pp.SkipTo(step_stmt)) + step_stmt
    mental_stmt = mental_abl
    precondition_stmt = precond_abl
    spec_stmt = s(specificity_abl) + NUM

    beh_ent_stmt.setParseAction(lambda x: AblEnt(x[0]))
    register_act_stmt.setParseAction(lambda x: AblRegistration(obj_e.ACT, x[0]))
    register_wme_stmt.setParseAction(lambda x: AblRegistration(obj_e.WME, x[0]))
    behavior_stmt.setParseAction(lambda x: AblBehavior(x['name'][0], x['args'][:] + [x['form']]))
    spawn_stmt.setParseAction(lambda x: AblComponent(obj_e.SPAWN, name=x[1], args=[x[0]]))
    step_stmt.setParseAction(lambda x: AblComponent(obj_e.STEP, x[0]))
    mental_stmt.setParseAction(lambda x: AblComponent(obj_e.MENTAL))
    precondition_stmt.setParseAction(lambda x: AblComponent(obj_e.PRECON))
    spec_stmt.setParseAction(lambda x: AblComponent(obj_e.SPEC, args=[float(x[0])]))
    initial_abl.setParseAction(lambda x: AblBehavior("initial_tree", [], True))

    pass_stmt = pp.restOfLine
    pass_stmt.setParseAction(lambda x: utils.ParseBase())

    abl_parser = pp.MatchFirst([beh_ent_stmt,
                                register_act_stmt,
                                register_wme_stmt,
                                behavior_stmt,
                                skip_to_spawn,
                                skip_to_step,
                                mental_stmt,
                                precondition_stmt,
                                spec_stmt,
                                pass_stmt])

    com_parser = pp.dblSlashComment
    com_parser.setParseAction(lambda x: { 'type' : 'comment'})

    return abl_parser, com_parser

def extract_from_file(filename, abl_parser, com_parser):
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
            result = abl_parser.parseString(current)[0]
            #Get open and close brackets
            #handle result:
            if not result:
                continue

            result._line_no = state['line']

            if isinstance(result, AblEnt):
                data['behaving_entity'] = result
            elif isinstance(result, AblRegistration):
                data['registrations'].append(result)
            elif isinstance(result, AblBehavior):
                state['current'] = result
                data['behaviors'].append(state['current'])
            elif isinstance(result, AblComponent):
                state['current'].add_component(result)
            elif not isinstance(result, utils.ParseBase):
                logging.warning("Unrecognised parse result: {}".format(result))

    return data


if __name__ == "__main__":
    abl_parser, com_parser = build_parser()
    files = utils.get_data_files([join("data","abl")], ".abl")
    for f in files:
        data = extract_from_file(f, abl_parser, com_parser)
        data_str = utils.convert_data_to_output_format(data, ["registrations", "behaviors"])
        utils.write_output(f, data_str, ".abl_analysis")