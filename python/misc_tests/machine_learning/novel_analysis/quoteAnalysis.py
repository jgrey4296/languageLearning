from __future__ import print_function
from __future__ import unicode_literals
import sys
import json
import os
import codecs
from spacy.en import English

#load a json of extracted speech quotes (from novelAnalysis)
#and currently just print them out


nlp = English()
sourceLocation = "../data/quotesInJson"


if __name__ == "__main__":
    fileName = "Horus Rising - Dan Abnett.json"
    jsonRep = None
    if len(sys.argv) > 1:
        fileName = sys.argv[1]

    print("Loading:",fileName)
    with codecs.open(os.path.join(sourceLocation,fileName),"r","utf-8") as f:
        jsonRep = json.loads(f.read())

    for x in jsonRep['quotes'][0:10]:
        doc = nlp(x['string'])
        print("Doc: ",x['string'])
        for token in doc:
            print(unicode(token)," : ",token.pos_)
        
    
