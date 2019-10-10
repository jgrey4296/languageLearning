from os.path import join, isfile, exists, isdir, splitext, expanduser
from os import listdir
import json
import IPython
import cif_types as cf

# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.cif_loader"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################


SOURCE_DIR = "/Users/jgrey/github/socialSimData/cif_data/"
locs = ["Games", "Stories"]
additional_files = [ "triggers", "library (trigger games)", "facadeState"]
ftype = ".json"

def parse_games(source_dir):
    logging.info("Parsing Games in: {}".format(source_dir))
    games = []
    for p in listdir(source_dir):
        h, t = splitext(p)
        if t != ftype:
            continue
        logging.info("Opening: {}".format(h))
        with open(join(source_dir, p)) as f:
            data = json.load(f)
        #load social game, and microtheories

        if 'Microtheories' in data['CiFLibraries'] and \
           len(data['CiFLibraries']['Microtheories']) != 0:
            logging.info("There are {} microtheories".format(len(data['CiFLibraries']['Microtheories']['Microtheory'])))
            
        assert('CiFLibraries' in data)
        assert('SocialGameLibrary' in data['CiFLibraries'])
        if len(data['CiFLibraries']['SocialGameLibrary']) == 0:
            logging.info("No Social Game")
            continue

        assert('SocialGame' in data['CiFLibraries']['SocialGameLibrary'])
        assert(isinstance(data['CiFLibraries']['SocialGameLibrary']['SocialGame'], dict))
            
        game = cf.makeSocialGame((data['CiFLibraries']['SocialGameLibrary']['SocialGame']))
        assert(isinstance(game, cf.SocialGame))
        games.append(game)

    return games

def parse_microtheories(source_dir):
    logging.info("Parsing Microtheories in: {}".format(source_dir))
    microtheories = []
    for p in listdir(source_dir):
        h, t = splitext(p)
        if t != ftype:
            continue
        logging.info("Opening: {}".format(h))
        with open(join(source_dir, p)) as f:
            data = json.load(f)

        if not 'Microtheories' in data['CiFLibraries'] or len(data['CiFLibraries']['Microtheories']) == 0 or len(data['CiFLibraries']['Microtheories']['Microtheory']) == 0:
            logging.info("No Microtheories")
            continue
        
        for d in data['CiFLibraries']['Microtheories']['Microtheory']:
            microtheory = cf.makeMicrotheory(d)
            assert(isinstance(microtheory, cf.Microtheory))
            microtheories.append(microtheory)

    return microtheories

if __name__ == "__main__":
    games = parse_games(join(SOURCE_DIR, locs[0]))
    microtheories = parse_microtheories(join(SOURCE_DIR, locs[0]))
    IPython.embed(simple_prompt=True)
