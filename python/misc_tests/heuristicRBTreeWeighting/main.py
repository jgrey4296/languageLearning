"""
    Red-Black Tree Heuristic Weighting test
"""
# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = ".log"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################
# IMPORTS
####################
import rbtree

##############################
# VARIABLES
####################
tree = rbtree.RBTree()

##############################
# Utilities
####################
class Value:
    """ A Value class that when compared, calls user input """
    def __init__(self,name="default"):
        self.name = name
        self.memoized_comparisons = {}

    def __lt__(self,b):
        if b.name in self.memoized_comparisons:
            return self.memoized_comparisons[b.name]
        logging.info("{} [<---->] {}".format(self.name,b.name))
        response = ''
        result = False
        while response != '<' and response != '>':
            response = input(':')
            if response == '<':
                result = True
            elif response == '>':
                result = False
        self.memoized_comparisons[b.name] = result
        return result

##############################
# Core Functions
####################

def coreloop():
    logging.info("RBtree Heuristic Weights Test")
    items_remaining = 10
    specified_items = set()
    pre_specified_items = []
    if input('Specify all items now? y/[n]') == 'y':
        pre_specified_items = input('Space Separated: ').split(' ')
        items_remaining = len(pre_specified_items)
    else:
        try:
            items_remaining = int(input('Number of Items to Insert: '))
        except ValueError as e:
            logging.info("Couldn't Read Specified Number, defaulting to 10")
        
    while len(tree) < items_remaining:
        if len(pre_specified_items) > 0:
            valueName = pre_specified_items.pop(0)
        else:
            valueName = input('New Item: ')
        if valueName in specified_items:
            logging.info("Item already Specified")
            continue
        tree.insert(Value(valueName))
        specified_items.add(valueName)

    logging.info("{} Items Specified".format(len(tree)))
    logging.info("Final Order:\n")
    raw_flattened = tree.flattened()
    value_wrappers = [x.value for x in raw_flattened]
    actual_names = [x.name for x in value_wrappers]
    logging.info(" < ".join(actual_names))


########################################
if __name__ == "__main__":
    logging.info("Starting ")
    coreloop()
