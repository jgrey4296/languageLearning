import urllib
from random import random
from time import sleep
from bs4 import BeautifulSoup
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir
import argparse
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

base_url = "https://wowwiki.fandom.com/"
index_file = "index.html"

if __name__ == "__main__":
    soup = None
    sleep_counter = 0
    sleep_at = 20
    sleep_max = 5
    sleep_min = 1

    logging.info("Starting Wow Download")
    #see https://docs.python.org/3/howto/argparse.html
    parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                     epilog = "\n".join([""]))
    parser.add_argument('-d', '--directory')
    args = parser.parse_args()
    args.directory = abspath(expanduser(args.directory))
    #load index
    with open(join(args.directory, index_file),'r') as f:
        soup = BeautifulSoup(f.read(), features="lxml")

    #extract sub pages
    quest_groups = [x.attrs['href'] for x in soup.find(class_="category-page__members").find_all("a")]

    #retrieve sub pages
    quest_group_sources = []
    logging.info("Retrieving {} quest groups".format(len(quest_groups)))
    for link in quest_groups:
        logging.info("Retrieving: {}".format(link))
        file_name = "{}.html".format(link.replace("/","_"))
        quest_group_sources.append(file_name)

        if exists(join(args.directory, file_name)):
            continue

        if sleep_counter >= sleep_at:
            sleep_counter = 0
            logging.info("...")
            sleep(sleep_min + sleep_max)
        else:
            sleep_counter += 1
            sleep(sleep_min + sleep_max * random())

        try:
            with open(join(args.directory, file_name), 'w') as target:
                with urllib.request.urlopen(base_url + link) as source:
                    target.write(source.read().decode("utf-8","ignore"))
            logging.info("Written: {}".format(file_name))
        except urllib.error.HTTPError as e:
            logging.warning("Problem: {}".format(str(e)))


    #extract individual quests
    all_quests = set()
    for group_source in quest_group_sources:
        with open(join(args.directory, group_source),'r') as f:
            soup = BeautifulSoup(f.read(), features="lxml")
            members = soup.find(class_="category-page__members")
            if bool(members):
                all_quests.update([x.attrs['href'] for x in members.find_all("a")])

    logging.info("Retrieving {} individual quests".format(len(all_quests)))
    with open(join(args.directory, 'all_quest_list.txt'), 'w') as f:
        f.write("\n".join(all_quests))

    all_remaining_quests = [x for x in all_quests if not exists(join(args.directory, "{}.html".format(x.replace("/", "_"))))]

    len_all_quests = len(all_remaining_quests)
    subtracted = len(all_quests) - len_all_quests
    logging.info("{} - {} = {} remaining quests".format(len(all_quests),
                                                        subtracted,
                                                        len_all_quests))
    #retrieve individual quests
    for i, link in enumerate(all_remaining_quests):
        logging.info("Retrieving {}/{} : {}".format(i, len_all_quests, link))
        file_name = "{}.html".format(link.replace("/","_"))
        quest_group_sources.append(file_name)
        if exists(join(args.directory, file_name)):
            continue

        if sleep_counter >= sleep_at:
            sleep_counter = 0
            logging.info("...")
            sleep(sleep_min + sleep_max)
        else:
            sleep_counter += 1
            sleep(sleep_min + sleep_max * random())

        try:
            with open(join(args.directory, file_name), 'w') as target:
                with urllib.request.urlopen(base_url + link) as source:
                    target.write(source.read().decode("utf-8","ignore"))
            logging.info("Written {}".format(file_name))
        except urllib.error.HTTPError as e:
            logging.warning("Url Error: {}".format(str(e)))


    #parse individual quests
