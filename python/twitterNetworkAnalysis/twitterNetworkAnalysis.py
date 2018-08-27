from twitter import *
from os.path import join, isfile, exists, isdir, splitext, expanduser
from os import listdir
from time import sleep
import IPython
import pickle
# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.networkAnalysis"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')
console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################
MY_TWITTER_CREDS = "my.credentials"
KEY_FILE = "consumer.key"
SECRET_FILE = "consumer.secret"

C_KEY = None
C_SECRET = None
TOKEN = None
TOKEN_SECRET = None

AMFP = "al_my_friends.pickle"
FMP = "friend_map.pickle"

fifteen_minutes = 60 * 15
def sleepAndSave(all_friends, friend_map, amnt=fifteen_minutes):
    """ Sleep for 15 minutes """
    logging.info("Saving then Sleeping")
    with open(AMFP, 'wb') as f:
        pickle.dump(all_friends, f)
    with open(FMP, 'wb') as f:
        pickle.dump(friend_map, f)
    sleep(amnt)

def load_if_exists():
    logging.info("Loading if files exist")
    all_my_friends = []
    friend_map = {}
    if exists(AMFP):
        with open(AMFP, 'rb') as f:
            all_my_friends = pickle.load(f)
    if exists(FMP):
        with open(FMP, 'rb') as f:
            friend_map = pickle.load(f)
    return all_my_friends, friend_map
    
def load_credentials_and_setup():
    """ Load the keys and tokens, and setup the twitter client """
    #Get the Key and Secret from (gitignored) files 
    assert(exists(KEY_FILE))
    assert(exists(SECRET_FILE))
    logging.info("Setting up Twitter Client")
    with open("consumer.key","r") as f:
        C_KEY = f.read().strip()
    with open("consumer.secret", "r") as f:
        C_SECRET = f.read().strip()

    if not exists(MY_TWITTER_CREDS):
        oauth_dance("jgNetworkAnalysis", C_KEY, C_SECRET, MY_TWITTER_CREDS)

    TOKEN, TOKEN_SECRET = read_token_file(MY_TWITTER_CREDS)
    assert(all([x is not None for x in [C_KEY, C_SECRET, TOKEN, TOKEN_SECRET]]))
    t = Twitter(auth=OAuth(TOKEN, TOKEN_SECRET, C_KEY, C_SECRET))
    return t


def get_friends(t, id=None):
    """ Given a twitter client, get my friends (ie: people I follow) 
    friends/ids returns up to 5000, 15 times in 15 minutes
    """
    logging.info("Getting friends for: {}, type: {}".format(id, type(id)))
    #Gives up to 5000
    if id is not None:
        response = t.friends.ids(user_id=id, stringify_ids="true")
    else:
        response = t.friends.ids(stringify_ids="true")
    logging.info("Response length: {}".format(len(response['ids'])))
    return response['ids']


def get_details(t, ids=None):
    """ Use users/lookup 300 times every 15 minutes on blocks of 100 users """ 
    








# while cursor != 0:
#         try:
#             temp = t.friends.list()
#             cursor = temp['next_cursor']
#             friends += temp['users']
#             time.sleep(60)
#         except TwitterHTTPError as e:
#             if e.response_data['errors'][0]['code'] == 88:
#                 logging.warning("Rate limit exceeded, sleeping")
#                 time.sleep(15 * 60);

# #Of use:
# danThat = t.users.lookup(screen_name='danthat')[0]
# danFriends = t.friends.ids(user_id=danthat['id_str'])
# danFriends_ids = danFriends['ids']
# dan_friend_one = t.users.lookup(user_id=danFriends[0])[0]

# try:
#     #something
#     t.users.lookup(user_id="doesnt exist")
# except TwitterHTTPError as e:
#     if e.response_data['errors'][0]['code'] == 88:
#         logging.warning("Rate limit exceeded")


# IPython.embed(simple_prompt=True)


if __name__ == "__main__":
    logging.info("TWITTER NETWORK ANALYSIS") 
    #Load data
    all_my_friends, friend_map = load_if_exists()
    
    #start the client
    t_client = load_credentials_and_setup()
    
    #get the people I follow
    if len(all_my_friends) == 0:
        all_my_friends = get_friends(t_client)

    #get the people followed by them
    if len(friend_map) == 0:
        friend_map = {x : set() for x in all_my_friends}
    count = 0

    logging.info("Starting to retrieve friends of friends")
    to_query = list(all_my_friends)
    while bool(to_query):
        x = to_query.pop(0)
        if len(friend_map[x]) != 0:
            continue
        try:
            xs_friends = get_friends(t_client, id=x)
            friend_map[x].update(xs_friends)
            count += 1
            sleep(30)
            if count > 14:
                sleepAndSave(all_my_friends, friend_map)
                count = 0
        except TwitterHTTPError as e:
            logging.info("Hit a Rate limit error, sleeping")
            to_query.append(x)
            sleepAndSave(all_my_friends, friend_map)
    
    #get all people followed
    all_friends = set([x for y in friend_map.values() for x in y])
    
    IPython.embed(simple_prompt=True)
    
    #flatten to get all ids
    logging.info("Getting all friend ids")
    all_ids = set(all_my_friends)
    for x in friend_map.values():
        all_ids.update(x)

    #save the all_ids set

    #get the details for all the ids
    logging.info("Starting to get details")
    names_and_tags = {}
    to_query = list(all_ids)
    count = 0
    while bool(to_query):
        xs = to_query[:100]
        try:
            details = get_details(t, xs)
            #integrate the details

            
            count += 1
            if count >= 300:
                sleepAndSave
        except TwitterHTTPError as e:
            logging.info("Hit a details rate limit, saving and sleeping")
            to_query += xs
            sleepAndSave
            
        
        
    
        
    
