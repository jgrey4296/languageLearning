"""
Very basic stats on loaded data
"""
import IPython
from csv_loading import load_d3_policies
import statGen
# Setup logging:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
logFileName = "simple_analysis.log"
root_logger.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)


filename = "policies.csv"
fieldsToCheck = "introduceCapital cancelCapital raiseCapital lowerCapital minCost maxCost implementationTime minIncome maxIncome".split(" ")


try:
    allPolicies = load_d3_policies(filename)
    #Get some basic stats on the policies:
    collectedStats = []
    for field in fieldsToCheck:
        logging.info("Getting stats for: {}".format(field))
        collectedStats.append(statGen.getStats(allPolicies, field))

    discreteStats = []
    discreteFieldsToCheck = "gui slider department".split(" ")
    for field in discreteFieldsToCheck:
        logging.info("Getting stats for: {}".format(field))
        discreteStats.append(statGen.getDiscreteStats(allPolicies,field))
except Exception as e:
    logging.exception(e)
    logging.exception("Failed on: {}".format(field))
    IPython.embed()
        
finally:
    IPython.embed()
