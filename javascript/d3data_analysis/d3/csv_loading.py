"""
Utilities to load data from democracy 3
"""
import IPython
from os.path import join, isfile
from collections import namedtuple
import csv
import numpy as np
from types import LambdaType
#from d3_data_types import Policy
from d3_data_classes import make_policy
import logging as root_logger
logging = root_logger.getLogger(__name__)

#CONSTANTS
BASEDIR = "/Users/jgrey/assets/Assets/gameData/democracy3"
AFRICA_BASEDIR = "Users/jgrey/assets/Assets/gameData/d3_africa"
LOAD_FILE = "policies.csv"
ENCODING = "iso-8859-1"
DELIM = ','
QUOTE = '"'

#UTILITY FUNCTIONS
def load_d3_csv(filename,dataCtorLambda,base_dir=BASEDIR):
    #loaded data array:
    all_data = []

    if not isfile(join(base_dir,filename)):
        logging.warning("File does not exist")
        raise Exception("File does not exist: {}".format(join(base_dir,filename)))

    logging.info("Loading file: {}".format(filename))
    with open(join(base_dir,filename),encoding=ENCODING) as f:
        data = csv.reader(f, delimiter=DELIM, quotechar=QUOTE)
        for i,row in enumerate(data): #lop off the csv header
            if i == 0:
                continue
            newData = dataCtorLambda(i,row)
            all_data.append(newData)
    return all_data

def load_d3_policies(file=LOAD_FILE):
    return load_d3_csv(file, lambda i,x: make_policy(i,*x))


