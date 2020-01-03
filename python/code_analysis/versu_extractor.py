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

    parallel_abl.setParseAction(lambda x: behavior_type_e.PAR)
    sequential_abl.setParseAction(lambda x: behavior_type_e.SEQ)

    beh_ent_stmt = s(be_abl) + NAME
    register_act_stmt = s(register_abl + act_abl) + NAME
    register_wme_stmt = s(register_abl + wme_abl) + NAME

    behavior_stmt = (op(atomic_abl) + op(joint_abl)).setResultsName("args") + pp.Or([sequential_abl, parallel_abl]).setResultsName('form') + s(behavior_abl) + pp.Group(NAME).setResultsName("name")
    spawn_stmt = pp.Or([spawn_abl, subgoal_abl]) + NAME
    mental_stmt = mental_abl
    precondition_stmt = precond_abl
    spec_stmt = s(specificity_abl) + NUM

    beh_ent_stmt.setParseAction(lambda x: { 'type' : obj_e.ENT,
                                            'name' : x[0] })

    register_act_stmt.setParseAction(lambda x: { 'type' : obj_e.ACT,
                                                 'name' : x[0]})

    register_wme_stmt.setParseAction(lambda x: { 'type' : obj_e.WME,
                                                 'name' : x[0]})

    behavior_stmt.setParseAction(lambda x: { 'type' : obj_e.BEH,
                                             'name' : x['name'][0],
                                             'args' : x['args'][:] + [x['form']]})

    spawn_stmt.setParseAction(lambda x: { 'type': obj_e.SPAWN,
                                          'name': x[1],
                                          'args' : x[0]})

    mental_stmt.setParseAction(lambda x: { 'type' : obj_e.MENTAL })

    precondition_stmt.setParseAction(lambda x: { 'type' : obj_e.PRECON })

    spec_stmt.setParseAction(lambda x: { 'type' : obj_e.SPEC,
                                         'args' : float(x[0]) })

    pass_stmt = pp.restOfLine
    pass_stmt.setParseAction(lambda x: { 'type' : None })

    abl_parser = pp.MatchFirst([beh_ent_stmt,
                                register_act_stmt,
                                register_wme_stmt,
                                behavior_stmt,
                                spawn_stmt,
                                mental_stmt,
                                precondition_stmt,
                                spec_stmt,
                                pass_stmt])

    com_parser = pp.dblSlashComment
    com_parser.setParseAction(lambda x: { 'type' : 'comment'})

def get_versu_files():
    logging.info("Getting Versu Files")
    files = []
    files = []
    base = ["data", "versu"]
    queue = [join(*base)]
    while bool(queue):
        current = queue.pop(0)
        if isfile(current) and splitext(current)[1] == ".praxis":
            files.append(current)
        elif isdir(current):
            sub = [join(current,x) for x in listdir(current)]
            queue += sub

    logging.info("Found {} versu files".format(len(files)))
    return files

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
    files = utils.get_data_files([join("data","versu")], [".type", ".data", "praxis"])
    for f in files:
        data = extract_from_file(f)
        data_str = utils.convert_data_to_output_format(data, [])
        utils.write_output(f, data_str, ".versus_analysis")
