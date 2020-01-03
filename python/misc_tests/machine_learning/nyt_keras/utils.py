from os.path import join, isfile, exists, isdir, splitext, expanduser
from os import listdir
import numpy as np
from collections import Counter, defaultdict
from sklearn.feature_extraction.text import TfidfVectorizer
import json
from random import choice
from keras.models import model_from_json
import loader
import spacy
import logging as root_logger
import pickle
import string
from collections import defaultdict
import IPython
logging = root_logger.getLogger(__name__)

nlp = spacy.load('en')

tfidf = TfidfVectorizer(input='content', use_idf=True, smooth_idf=True)
ddict = lambda: defaultdict(lambda: 0)
vocab = set(string.printable)   
chars = sorted(list(vocab))
char_indices = defaultdict(lambda: 0)
indices_char = defaultdict(lambda: " ")

for i,c in enumerate(chars):
    char_indices[c] = i+1
    indices_char[i+1] = c

    
#For use with spacy:
def sp_word_set(text):
    words = [word.string.strip().lower() for word in text]
    return set(words)

def sp_ngrams(text, n):
    words = [word for word in text if word.is_alpha and not word.string.isupper()]
    return list(zip(*[words[i:] for i in range(n)]))

def sp_ngrams_lc(text, n):
    words = [word.string.strip().lower() for word in text if word.is_alpha]
    return list(zip(*[words[i:] for i in range(n)]))

def sp_ngrams_count(ngrams):
    #todo: enable weighting the ngrams?
    return Counter(ngrams)

def sp_ngrams_set(text, n):
    return set(sp_ngrams(text, n))

def sp_ngrams_dicts(text, ngram_size=3):
    all_dicts = {}
    for i in range(2, ngram_size+1):
        ngs = sp_ngrams_lc(text,i)
        cngs = sp_ngrams_count(ngs)
        #todo: instead of ddict could be a counter?
        ngs_dict = { ' '.join(x[0:i-1]) : ddict() for x in ngs }
        for x in cngs:
            ngs_dict[' '.join(x[0:i-1])][x[i-1]] = cngs[x]
        all_dicts[i] = ngs_dict

    return all_dicts

def sp_ngram_generate(ngram_dict, length=10):
    #todo: add backoff, weighted selection
    #choose a random starting string from the largest ngram dict:
    size = max(ngram_dict.keys())
    l_dict = ngram_dict[size]
    text = choice(list(l_dict.keys()))
    for i in range(length):
        words = text.split(' ')
        text += " " + choice(list(l_dict[" ".join(words[-(size-1):])].keys()))
    return text


#For neural nets:
def nn_map_text(text):
    """ Given some text, create a mapping to integers and back """
    #todo: enable it to work for tokens as well 
    chars = sorted(list(set(text)))
    char_indices = dict((c, i) for i, c in enumerate(chars))
    indices_char = dict((i, c) for i, c in enumerate(chars))
    return (char_indices, indices_char)

def nn_map_sample(predictions, temperature=1.0):
    """ For a word mapping M:{i : char} dictionary, give [] of len(M) of predictions of 
    the next index. Normalize it and sample, taking the highest. Return that index """
    #cast to high precision?
    preds = np.asarray(predictions).astype('float64')
    preds = np.log(preds) / temperature
    exp_preds = np.exp(preds)
    #normalize
    preds = exp_preds / np.sum(exp_preds)
    probabilities = np.random.multinomial(1, preds, 1)
    return np.argmax(probabilities)

def nn_save_model(model, filename):
    model_json = model.to_json()
    with open("{}.json".format(filename), 'w') as f:
        f.write(model_json)
    model.save_weights("{}.h5".format(filename))

def nn_load_model(filename):
    with open('{}.json'.format(filename), 'r') as f:
        data = f.read()
    loaded_model = model_from_json(data)
    loaded_model.load_weights("{}.h5".format(filename))
    return loaded_model

#Nyt data
def nyt_flatten(data):
    assert(isinstance(data, list))
    assert(isinstance(data[0], list))
    total = []
    for x in data:
        total += x
    return total


#glove loading
def glove_load(filename):
    output = {}
    if not exists(filename):
        raise Exception("Glove loading: File doesnt exist")
    with open(filename) as f:
        data = f.readlines()
    for line in data:
        splitLine = line.strip().split(" ")
        word = splitLine[0]
        vec = np.array([float(x) for x in splitLine[1:]])
        output[word] = vec
    return output


def glove_verify_vocab(glove_dict, vocab):
    assert(isinstance(glove_dict, dict))
    assert(isinstance(vocab, set))
    return all([x in glove_dict for x in vocab])



def create_training_data(dir, num_documents=5, MAXLEN=5, STEP=3, vocab_file=None):
    # if exists(vocab_file):
    #     with open(vocab_file, 'rb') as f:
    #         vocab = pickle.load(f)
    # else:
    #     vocab = set()

    
    data = []
    #Get the Text data
    for x in range(num_documents):
        data.append(loader.get_title_and_paragraph(dir, rand=True))
    
    flattened = nyt_flatten(data)
    all_titles = [x[0] for x in flattened]
    logging.info("Corpus Length: {}".format(len(data)))
    for i,x in enumerate(data):
        logging.info("Doc {}: len {}".format(i, len(x)))


    #vocab = vocab.union(set(" ".join(all_titles)))
    # with open(vocab_file, 'wb') as f:
    #     pickle.dump(vocab, f)
    
    #chars = sorted(list(vocab))
        
    #Chop up text into training data
    sentences = []
    next_char = []
        
    for i in range(0, len(all_titles)):
        current_title = all_titles[i].lower()
        for j in range(0, len(current_title) - MAXLEN, STEP):
            #The sequence of text. Training Data.
            sentences.append(current_title[j: j + MAXLEN])
            #the char that comes after. True result
            next_char.append(current_title[j + MAXLEN])
            
    logging.info("Sequences: {}".format(len(sentences)))
                
    #setup input and output vectors
    # n samples, of length i sequences, of length j vectors
    X = np.zeros((len(sentences), MAXLEN, 1+len(chars)), dtype=np.bool)
    # n samples, of a single length j vector
    y = np.zeros((len(sentences), 1+len(chars)), dtype=np.bool)
    
    #populate the data
    for i, sentence in enumerate(sentences):
        for t, char in enumerate(sentence):
            #Training data of the current word
            try:
                X[i, t, char_indices[char]] = 1
            except IndexError:
                IPython.embed(simple_prompt=True)
            #the output true result
            y[i, char_indices[next_char[i]]] = 1
            

    return (X, y, chars, char_indices, indices_char, all_titles)
                        
                        
