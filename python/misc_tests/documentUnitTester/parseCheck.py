"""
A Quick file to check text can be parsed
"""
# Setup root_logger:
import IPython
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "parseCheck.log"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################
from doctester import TextParser as tp

with open('data/test.org') as f:
    text = f.read()

r = tp.parseText(text)


IPython.embed(simple_prompt=True)
