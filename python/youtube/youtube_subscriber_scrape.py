# -*- coding: utf-8 -*-

# Sample Python code for youtube.channels.list
# See instructions for running these code samples locally:
# https://developers.google.com/explorer-help/guides/code_samples#python
# Setup root_logger:
import os
import json
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir
import google_auth_oauthlib.flow
import googleapiclient.discovery
import googleapiclient.errors
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################
# see https://developers.google.com/youtube/v3/docs/videos/list

# Dot Files:
SUB_DATA = ".subscription_data"
CHAN_DATA = ".channel_data"
VIDEO_DATA = ".video_data"

scopes = ["https://www.googleapis.com/auth/youtube.readonly"]


def get_my_subscriptions(youtube, sub_state):
    logging.info("Getting My Subscriptions")

    if sub_state['last_page_token'] is None:
        logging.info("Initial Subscription Request")
        request = youtube.subscriptions().list(mine=True,
                                               part="snippet",
                                               maxResults=50)
        response = request.execute()
        logging.debug(response)
        extracted_data = {x['snippet']['resourceId']['channelId']: x['snippet']['title'] for x in response['items']}
        sub_state['subscriptions'].update(extracted_data)
        sub_state['total'] = response['pageInfo']['totalResults']
        logging.info("Total Subscription Target Count: {}".format(sub_state['total']))

        if 'nextPageToken' in response:
            sub_state['last_page_token'] = response['nextPageToken']
            logging.info("Page Token: {}".format(sub_state['last_page_token']))

    while sub_state['last_page_token'] is not None:
        request = youtube.subscriptions().list(mine=True,
                                               part="snippet",
                                               pageToken=sub_state['last_page_token'],
                                               maxResults=50)
        response = request.execute()
        extracted_data = {x['snippet']['resourceId']['channelId']: x['snippet']['title'] for x in response['items']}
        sub_state['subscriptions'].update(extracted_data)
        if 'nextPageToken' in response:
            sub_state['last_page_token'] = response['nextPageToken']
        else:
            sub_state['last_page_token'] = None
        logging.info("Page Token: {}".format(sub_state['last_page_token']))

    return sub_state


def get_channel_uploads(youtube, channel_ids, chan_state):
    logging.info("Getting Channel Upload playlists ({})".format(len(channel_ids)))

    for channel_id in channel_ids:
        if channel_id in chan_state['playlists']:
            continue

        request = youtube.channels().list(part='contentDetails', id=channel_id)
        response = request.execute()
        logging.debug(response)
        if bool(response['items']):
            uploads_playlist = response['items'][0]['contentDetails']['relatedPlaylists']['uploads']
            chan_state['playlists'][channel_id] = uploads_playlist
        else:
            logging.info("No response?")
            breakpoint()


    return chan_state


def get_videos_for_playlist(youtube, playlist_id, video_state):
    logging.info("Getting videos for playlist: {}".format(playlist_id))

    video_state['last_playlist'] = playlist_id
    if playlist_id not in video_state['videos']:
        video_state['videos'][playlist_id] = []

    if video_state['last_page_token'] is None:
        request = youtube.playlistItems().list(part="snippet", playlistId=playlist_id, maxResults=50)
        response = request.execute()
        video_state['totals'][playlist_id] = response['pageInfo']['totalResults']

        for video in response['items']:
            video_state['videos'][playlist_id].append(make_vid_data(video))

        logging.info("Got {}/{} videos".format(len(video_state['videos'][playlist_id]), video_state['totals'][playlist_id]))

        if 'nextPageToken' in response:
            video_state['last_page_token'] = response['nextPageToken']

    while video_state['last_page_token'] is not None:
        request = youtube.playlistItems().list(part="snippet",
                                               playlistId=playlist_id,
                                               pageToken=video_state['last_page_token'],
                                               maxResults=50)
        response = request.execute()

        for video in response['items']:
            video_state['videos'][playlist_id].append(make_vid_data(video))

        logging.info("Got {}/{} videos".format(len(video_state['videos'][playlist_id]), video_state['totals'][playlist_id]))

        if 'nextPageToken' in response:
            video_state['last_page_token'] = response['nextPageToken']
        else:
            video_state['last_page_token'] = None

    return video_state


def make_vid_data(video):
    vid_data = {}
    vid_data['id'] = video['id']
    vid_data['title'] = video['snippet']['title']
    vid_data['description'] = video['snippet']['description']
    vid_data['publish_date'] = video['snippet']['publishedAt']
    return vid_data


def main():
    logging.info("Starting Youtube analysis")
    # Disable OAuthlib's HTTPS verification when running locally.
    # *DO NOT* leave this option enabled in production.
    os.environ["OAUTHLIB_INSECURE_TRANSPORT"] = "1"

    with open('.key', 'r') as f:
        key = f.read()

    api_service_name = "youtube"
    api_version = "v3"
    client_secrets_file = ".secrets.json"

    # Get credentials and create an API client
    flow = google_auth_oauthlib.flow.InstalledAppFlow.from_client_secrets_file(
        client_secrets_file, scopes)
    credentials = flow.run_console()
    youtube = googleapiclient.discovery.build(
        api_service_name,
        api_version,
        credentials=credentials,
        developerKey=key,
        cache_discovery=False)


    sub_state = {'total': -1,
                 'subscriptions': {},
                 'last_page_token': None}
    chan_state = {'playlists': {}}
    video_state = {'videos': {},
                   'totals': {},
                   'last_playlist': None,
                   'last_page_token': None}

    if exists(SUB_DATA):
        with open(SUB_DATA, 'r') as f:
            sub_state.update(json.load(f))

    if exists(CHAN_DATA):
        with open(CHAN_DATA, 'r') as f:
            chan_state.update(json.load(f))

    if exists(VIDEO_DATA):
        with open(VIDEO_DATA, 'r') as f:
            video_state.update(json.load(f))

    try:
        # Get any remaining subscriptions
        if sub_state['total'] == -1 or len(sub_state['subscriptions']) < sub_state['total']:
            sub_state = get_my_subscriptions(youtube, sub_state)

        # Now get all the channel's "uploads" playlist
        chan_state = get_channel_uploads(youtube,
                                         sub_state['subscriptions'].keys(),
                                         chan_state)

        chan_state['reverse_lookup'] = {x[1]:x[0] for x in chan_state['playlists'].items()}

        # now finish any unfinished video retrieval:
        if video_state['last_playlist'] is not None and video_state['last_page_token'] is not None:
            logging.info("Continuing: {}".format(video_state['last_playlist']))
            video_state = get_videos_for_playlist(youtube,
                                                  video_state['last_playlist'],
                                                  video_state)

        # and continue with normal iteration to get videos:
        for channel_id, playlist_id in chan_state['playlists'].items():
            if playlist_id not in video_state['videos'] or len(video_state['videos'][playlist_id]) < video_state['totals']:
                video_state = get_videos_for_playlist(youtube, playlist_id, video_state)

        # If theres a problem, break
    except Exception as exp:
        logging.warning("Exception occurred: {}".format(exp))
        breakpoint()
    finally:
        logging.info("Writing out data")
        # always Write the states
        with open(SUB_DATA, 'w') as f:
            json.dump(sub_state, f)

        with open(CHAN_DATA, 'w') as f:
            json.dump(chan_state, f)

        with open(VIDEO_DATA, 'w') as f:
            json.dump(video_state, f)


if __name__ == "__main__":
    main()
