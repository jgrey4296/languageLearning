"""
Get xml files of text (usc, the bible etc) from data dir,

output to similarly named files in analysis directory
"""
from enum import Enum
from os import listdir
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from random import shuffle
import csv
import pyparsing as pp
import re
import spacy
import utils

#Choose:
#https://docs.python.org/3/library/xml.etree.elementtree.html
import lxml
from bs4 import BeautifulSoup


nlp = spacy.load("en_core_web_sm")
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
KJV_RE = re.compile('kjv([0-9]+)(O|A)z([0-9]+)z([0-9]+)')



def extract_from_file(filename):
    logging.info("Extracting from: {}".format(filename))
    data = { }

    with open(filename, 'rb') as f:
        text = f.read().decode('utf-8','ignore')

    soup = BeautifulSoup(text, features='lxml')

    if 'wow_quests' in filename:
        data = parse_wow_quest(soup)
    elif "trump" in filename:
        data = parse_trump(soup)
    elif "kjv" in filename:
        data = parse_bible(soup)
    elif "usc" in filename:
        data = parse_usc(soup)
    elif "dragon" in filename:
        data = parse_king_dragon_pass(soup)
    elif "roberts" in filename:
        data = parse_roberts_rules(soup)
    elif "twine" in filename:
        data = parse_twine(soup)
    elif "unrest" in filename:
        data = pase_unrest(soup)
    else:
        data = utils.xml_search_components(data, soup, [x.name for x in soup.contents])


    return data

def parse_wow_quest(soup):
    logging.info("Parsing Wow Quest")
    data = {}

    wikiapage = soup.find(id="WikiaPage")
    assert(wikiapage is not None)
    data['title'] = wikiapage.find("h1").text

    main_container = wikiapage.find(id="mw-content-text")
    assert(main_container is not None)
    aside = main_container.find("aside")
    assert(aside is not None)
    aside_details = aside.find_all("div",class_="pi-item")
    for detail in aside_details:
        pair = detail.findChildren(recursive=False)
        assert(len(pair) >= 2)
        data[pair[0].text] = [x.get_text(", ") for x in pair[1:]]

    content = [x for x in main_container.findChildren(recursive=False) if x.name in ["h2", "p", "table", "ul", "ol"]]
    #Get rid of cruft at the start
    while content[0].name != 'h2':
        content.pop(0)
    h2s = [x for x in content if x.name == "h2"]
    ids = [y.attrs['id'] for x in h2s for y in x.findChildren(recursive=False) if 'id' in y.attrs]

    #loop through content, assigning p's to h2s
    curr_section = None
    while bool(content):
        current = content.pop(0)
        if current.name == "h2":
            curr_section = current.find('span').text
            data[curr_section] = []
        elif current.name == "p":
            assert(curr_section is not None)
            data[curr_section].append(current.text)
        elif current.name == "ul":
            lis = current.find_all('li',recursive=False)
            for li in lis:
                text = li.get_text()
                img = li.find('img')
                if img and 'alt' in img.attrs:
                    text += " {}".format(img.attrs['alt'])
                data[curr_section].append(text)

        elif current.name == "table":
            links = current.find_all('a')
            link_texts = [x.text for x in links if x.text != ""]
            data[curr_section].append(link_texts)
        elif current.name == "ol":
            lis = current.find_all('li', recursive=False)
            li_text = [x.text for x in lis]
            data[curr_section].append(li_text)

    return data

def parse_trump(soup):
    data = {}

    posts = soup.find_all("article")
    data['_num_posts'] = len(posts)
    data['days_mentioned'] = []
    data['components'] = set()
    for post in posts:
        day = post.find('h2').find('a').attrs['href']
        logging.info("Processing post: {}".format(day))

        for sub_structure in post.findChildren():
            data['components'].add(sub_structure.name)

        data[day] = {}
        data[day]['date'] = post.find('time').attrs['datetime']
        data['days_mentioned'].append(data[day]['date'])

        paras = post.find_all('p', recursive=False)
        texts = [x.text for x in paras]
        data[day]['paragraphs'] = texts
        links = [y.attrs['href'] for x in paras for y in x.find_all('a')]
        data[day]['links'] = links

    return data

def parse_bible(soup):
    data = {}
    divs = soup.find_all('divs')

    for section in divs:
        book, testament, chapter, verse = KJV_RE.match(section.find(h1).string).groups()
        text = nlp(section.find('p').string)

        #TODO

    return data

def parse_usc(soup):
    data = {}


    return data

def parse_king_dragon_pass(soup):
    data = {}

    #TODO

    return data

def parse_roberts_rules(soup):
    data = {}

    body = soup.find('body')

    paragraphs = body.find_all('p')
    #TODO


    return data

def parse_unrest(soup):
    data = {}

    #TODO


    return data

if __name__ == "__main__":
    base = ["data", "xml"]
    queue = [join(*base, x) for x in ["uscode",
                                      "king_james_bible",
                                      "red_shirt",
                                      "king_dragon_pass",
                                      "dwarf_fortress",
                                      "unrest",
                                      "twine"]]
    input_ext = [".xml", ".html"]
    output_lists = []
    output_ext = ".xml_text_analysis"

    utils.standard_main(queue,
                        input_ext,
                        extract_from_file,
                        output_lists,
                        output_ext)
