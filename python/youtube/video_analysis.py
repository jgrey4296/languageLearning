"""
Using all the videos retrieved, start to group them
"""
import datetime
import json
from os import listdir
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
import argparse

import spacy

# https://spacy.io/usage/
# nlp = spacy.load('en_core_web_lg')
nlp = spacy.load('en_core_web_sm')

# https://textblob.readthedocs.io/en/dev/quickstart.html
from textblob import TextBlob

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
date_format = "%Y-%m-%d"

def get_date(datestring):
    return datetime.datetime.strptime(datestring.split("T")[0], date_format)

def get_jsons(directory):
    return [join(directory,x) for x in listdir(directory) if splitext(x)[1] == ".json"]




if __name__ == "__main__":
    logging.info("Starting Video Analysis")
    #see https://docs.python.org/3/howto/argparse.html
    parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                     epilog = "\n".join([""]))
    parser.add_argument('-d', '--dir', default="videos")

    args = parser.parse_args()
    #args.aBool...

    with open('.subscription_data','r') as f:
        subscriptions = json.load(f)

    jsons = get_jsons(args.dir)

    dates = {}
    count = 0
    titles = []

    #Load each json
    for x in jsons:
        id_str = split(x)[1].split('.')[0]
        logging.info("Loading: {}".format(subscriptions['subscriptions'][id_str]))
        with open(x,'r') as f:
            data = json.load(f)
        assert(isinstance(data, list))

        #For all videos
        for vid in data:
            date = get_date(vid['publish_date'])
            if date.year not in dates:
                dates[date.year] = {'__counts': 0}
            if date.month not in dates[date.year]:
                dates[date.year][date.month] = {'__counts': 0}
            if date.day not in dates[date.year][date.month]:
                dates[date.year][date.month][date.day] = {'__counts': 0}

            #Count them by date
            dates[date.year]['__counts'] += 1
            dates[date.year][date.month]['__counts'] += 1
            dates[date.year][date.month][date.day]['__counts'] += 1
            dates[date.year][date.month][date.day][vid['id']] = True
            count += 1
            titles.append(vid['title'])

    logging.info("Total Counts: {}".format(count))
    with open('.all_names','w') as f:
        for x in titles:
            f.write("{}\n".format(x))

    with open('.dates','w') as f:
        json.dump(dates,f)

    breakpoint()
