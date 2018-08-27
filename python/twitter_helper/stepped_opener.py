"""
A means to open archived tweets, and open the tweets in batches 
of 10 to bookmark

"""
import json
import pickle
from subprocess import run
from os import listdir
from os.path import exists,isfile,join
import logging
import random

SET_STORE = "tweet_id_set"
TWEET_DIR = "data"
data = []
bookmarked_tweet_set = None
SEARCH_ENGINE = "https://duckduckgo.com/?q="
BATCH_NUM = 10
TOR_CALL = "open -a TorBrowser {}"

#Setup logging
LOGLEVEL = logging.DEBUG
logFileName = "tweets.log"
logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
console = logging.StreamHandler()
console.setLevel(logging.INFO)
logging.getLogger('').addHandler(console)

#load the previously bookmarked set
try:
    if exists(SET_STORE):
        with open(SET_STORE,'rb') as f:
            bookmarked_tweet_set = pickle.load(f)
    else:
        logging.debug("Starting with empty set ")
        bookmarked_tweet_set = set()
except Exception as e:
    logging.exception("set store reading failure",e)

#load the tweet data for all files:
try:
    logging.debug("Loading files")
    for filename in listdir(TWEET_DIR):
        logging.debug("File: {}".format(filename))
        with open(join(TWEET_DIR,filename)) as f:
            f.readline() #ignore the first line
            jdata = json.loads(f.read()) #json parse the rest
            logging.debug("Found {} tweets".format(len(jdata)))
            data = data + jdata
except Exception as e:
    logging.exception("File read error: ", e)
    raise e

#Filter the data based on the loaded set:
logging.debug('Filtering data')
focusData = [x for x in data if x['id_str'] not in bookmarked_tweet_set]
logging.info("Data filtered, size difference of: {}".format(len(data)-len(focusData)))

#utility function to save the set
def save_set(i):
    if i%10!=0:
        return
    logging.info("Achieved index: {}, saving set".format(i))
    try:
        with open(SET_STORE,'wb') as f:
            pickle.dump(bookmarked_tweet_set,f)
    except Exception as e:
        logging.exception("Saving set failed".format(i),e)
                        

def constructSearch(string):
    return SEARCH_ENGINE + "'{}'".format(string.replace(' ','+'))

def trigger_search(string):
    if string is not None:
        searchString = constructSearch(string)
        logging.debug("Searching for: {}".format(searchString))
        run([TOR_CALL.format(searchString)],shell=True)

def open_url(url):
    if url is not None:
        logging.debug("Opening: {}".format(url))
        run([TOR_CALL.format(url)],shell=True)
    
def split_into_text_and_url(tweet):
    """
    Each tweet has text and a url. split them apart,
    return as a new tuple
    """
    tweet_text = tweet['text']
    try:
        tweet_url = tweet['entities']['urls'][0]['url']
        tweet_text = tweet_text.replace(tweet_url,'')
    except KeyError as e:
        logging.debug("Key Error, so presumably no url, for: {}".format(tweet_text))
        return (tweet_text,None)
    except IndexError as e:
        logging.debug("Index Error, so presumably no url, for: {}".format(tweet_text))
        return (tweet_text,None)
    except Exception as e:
        logging.exception("Unexpected Exception: ",e)

    logging.debug("URL: {}".format(tweet_url))
    logging.debug("TEXT: {}".format(tweet_text))
    return (tweet_text,tweet_url)
    
    
#the counter to track numbers processed:
counter = 0
while len(focusData) > 0:
    logging.debug("---------- {}".format(counter))
    tweet = random.choice(focusData)
    if tweet['id_str'] in bookmarked_tweet_set:
        while tweet['id_str'] in bookmarked_tweet_set:
            logging.Exception('Tweet already bookmarked')
            focusData.remove(tweet)
            tweet = random.choice(focusData)
    logging.debug("Processing tweet: {}".format(tweet['id_str']))
    tt,tu = split_into_text_and_url(tweet)
    
    open_url(tu)
    trigger_search(tt)
 
    response = input("({}) Waiting.... ".format(counter))
    while(response != "" and response != "q"):
        if response == "r":
            #Re open the last tweet
            trigger_search(tt)
            open_url(tu)
        else:
            logging.info("Unrecognised command")
        response = input("({}) Waiting_b.... ".format(counter))

    if response == "q":
        #finish  
        logging.debug("Got Response: {}".format(response))
        logging.debug("Breaking at tweet: {}".format(tweet['id_str']))
        break;

    bookmarked_tweet_set.add(tweet['id_str'])
    focusData.remove(tweet)
    save_set(counter)
    counter += 1
    
logging.debug("Finishing")
