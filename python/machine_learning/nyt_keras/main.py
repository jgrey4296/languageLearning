#Based on https://github.com/fchollet/keras/blob/master/examples/lstm_text_generation.py
# Setup root_logger:
import logging as root_logger
import IPython
from keras.models import Sequential
from keras.layers import Dense, Activation
from keras.layers import LSTM
from keras.optimizers import RMSprop
from keras.utils.data_utils import get_file
import numpy as np
import random
import sys
import utils
import loader
#import spacy
from os.path import join, isfile, exists, isdir, splitext, expanduser
from os import listdir
import pickle
import sys


#------------------------------
#LOGGING Setup
#------------------------------
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "training.log"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')
console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
#------------------------------
# PARAMETER Setup
#------------------------------
#nlp = spacy.load('en')

MAXLEN = 40
STEP = 20
LEARN_RATE = 0.001
LEARN_RATE_DECREASE = 0.01
ITERATIONS = 40
EPOCHS = 5
BATCH_SIZE = 256
CORPUS_LENGTH = 5
GEN_DIVERSITY = [0.2, 0.5, 1.0, 1.2]
GENERATE_LENGTH = 150
NUM_EXAMPLES = 5

pratchett = "/users/jgrey/assets/Assets/Texts/Books/Discworld"
nyt = "/users/jgrey/github/nytimes_scraper/data"
nyt_win = "F:/data/nyt"
glove = "/users/jgrey/assets/Assets/glove.6B"


DATA_SOURCE = nyt_win

MODEL_FILE = "lstm_model_file"
VOCAB_FILE = "vocab"
OUTPUT = "output.txt"

args = sys.argv
skip_training = 'skip' in args

#------------------------------
# DATA setup
#------------------------------
logging.info("Loading Data")


X, y, chars, char_indices, indices_char, all_titles= utils.create_training_data(DATA_SOURCE,
                                                                     num_documents=CORPUS_LENGTH,
                                                                     MAXLEN=MAXLEN,
                                                                     STEP=STEP,
                                                                     vocab_file=VOCAB_FILE)



#------------------------------
# MODEL Setup
#------------------------------
logging.info("Setting up Model")
if not exists("{}.json".format(MODEL_FILE)):
    model = Sequential()
    model.add(LSTM(512, input_shape=(MAXLEN, 1+len(chars)), return_sequences=True, unroll=True, implementation=2))
    model.add(LSTM(256, return_sequences=True, unroll=True, implementation=2, dropout=0.2, recurrent_dropout=0.2))
    model.add(LSTM(512, unroll=True, implementation=2, dropout=0.2, recurrent_dropout=0.2))
    model.add(Dense(1+len(chars)))
    model.add(Activation('softmax'))
else:
    logging.info("Loading Model from File")
    model = utils.nn_load_model(MODEL_FILE)
    
optimizer = RMSprop(lr=LEARN_RATE)
model.compile(loss='categorical_crossentropy', optimizer=optimizer)



def generate_text():
    #Get a random starting character from the text
    start_index = random.randint(0, 1+len(chars))

    #Generate an example output:
    for diversity in GEN_DIVERSITY:
        for n in range(NUM_EXAMPLES):
            logging.info("\n---------- Diversity: {}".format(diversity))

            generated = []
            #Get the real text 
            sentence = all_titles[random.randrange(0, len(all_titles))][:MAXLEN]
            generated += sentence

            logging.info("Gen with Seed:\n{}\n-----".format(sentence))

            for i in range(GENERATE_LENGTH):
                #Create the input
                x = np.zeros((1, MAXLEN, 1+len(chars)), dtype=np.bool)
                startpos = max(MAXLEN - len(sentence) - 1, 0)
                for t, char in enumerate(sentence[-MAXLEN:]):
                    x[0, startpos+t, char_indices[char]] = 1


                #feed the input to the net:
                preds = model.predict(x, verbose=0)[0]
                #Reconstruct the character:
                next_index = utils.nn_map_sample(preds, diversity)
                next_char = indices_char[next_index]
                #logging.info("Generated char: {}".format(next_char))
                #add it to the generated string:
                generated.append(next_char)
                #move the sentence forward a character for the next loop
                sentence += next_char

            logging.info("Generated:\n{}\n".format("".join(generated)))
        
            with open(OUTPUT, 'a') as f:
                f.write("{}\n".format("".join(generated)))


#------------------------------
# TRAINING
#------------------------------

logging.info('Starting training')

if skip_training:
    logging.info("Skipping Training to generate text")
    generate_text()
else:
    for iteration in range(1, 1+ITERATIONS):
        logging.info("\n---------------Iteration: {}".format(iteration))
        #Train the model
        X, y, a, b, c, titles = utils.create_training_data(DATA_SOURCE,
                                                   num_documents=CORPUS_LENGTH,
                                                   MAXLEN=MAXLEN,
                                                   STEP=STEP,
                                                   vocab_file=VOCAB_FILE)
                                                                         
        model.fit(X,y,
                  batch_size=BATCH_SIZE,
                  epochs=EPOCHS)
        utils.nn_save_model(model, MODEL_FILE)
    
        #After Each Training Epoch, try out the model:
        generate_text()




    
