# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.authoringTest"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################
import IPython

def freeAuthor(defs):
    logging.info("Starting Free Authoring")
    i = ""
    while i != "q":
        i = input(": ")
        #add in facts, rules etc


    return defs


def suggestionAuthor(defs):
    logging.info("Starting Suggestion Authoring")
    i = ""
    while i != "q":
        i = input(": ")
        #suggest something to author and do it


    return defs


def explore(defs):
    logging.info("Starting Exploration")
    i = ""
    while i != "q":
        #starting at root, print parent, current, children

        #be able to jump into free author or suggestion for current node

    return defs


if __name__ == "__main__":
    #Todo: definitions should be a trie?
    definitions = {}
    #loop
    #choose: Authoring (Free / Suggested) or Exploration
    i = ""

    while (i != 'q'):
        i = input("Choose mode: Free Authoring (f) | Suggestion (s) | Exploration (e) | quit (q):")
        if i == 'f':
            #----------Authoring: Free
            #define a fact, rule, action, range
            definitions = freeAuthor(definitions)

        elif i == 's':
            #----------Authoring: Suggested
            #look at currently defined things
            #pick one that is under-defined (not used in N conditions / actions, has under M value variations)
            #ask for a rule / definition / use / annotation
            definitions = suggestionAuthor(definitions)

        elif i == 'e':
            #----------Exploration
            #everything is ina trie, so move layer by layer to explore, with the ability to shift to authoring
            definitions = explore(defs)


    #endloop
    logging.info("Finished Authoring")
    IPython.embed(simple_prompt=True)
