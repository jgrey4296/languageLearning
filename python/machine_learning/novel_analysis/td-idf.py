from __future__ import print_function
from nltk.corpus import PlaintextCorpusReader
from nltk.text import TextCollection

#load all the files in the corpus root,
#and calculate tf, idf, and tf_idf on them, and on a specific term

if __name__ == "__main__":
    corpus_root = '../data/source_data'
    corpus = PlaintextCorpusReader(corpus_root,'[a-zA-Z \-]*\.txt')

    ids = corpus.fileids()

    collection = TextCollection(corpus)

    #for x,word in enumerate(corpus.words(ids[0])[:200]):
    #    print(x,word)

    source = ids[0]
    term = corpus.words(source)[107]
    doc = corpus.words(ids[2])



    print("Source: ",source)
    print("TF of: ",term,": ",collection.tf(term,doc))
    print("IDF of: ",term,": ",collection.idf(term))
    print("tf_Idf of:",term,": ",collection.tf_idf(term,doc))

