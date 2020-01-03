from __future__ import unicode_literals
import sys
import re
import os
from textblob import TextBlob
import codecs
import json

#Main novel analysis. From a particular sourcelocation,
#load a book. get the location of a set of specific unicode
#chars, and use those to get all the quoted speech in the file
#export the quotes as json, ALONG with their context on either
#side for an amount of context. defaults to using the u2018 as
#a start point to pay attention to


#Relation to data is: ../data/parsed_data/
#file: Night Watch - Terry Pratchett.txt
sourceLocation = '../data/source_data/40k'

#Specific values to use for quote detection
START = u'\u2018'
STOP = u'\u2019'

ALTSTART = u'\u201C'
ALTSTOP = u'\u201D'

ASCIISTART = u'\u0060'
ASCIIQUOTE = u'\u0022'

class Novel_Analysis:

    def __init__(self,loadFileName,focusValue):
        self.fileName = loadFileName
        self.focusValue = focusValue
        self.outputLocation = "../data/quotesInJson"
        self.outputJsonName = loadFileName.replace(".txt",".json")
        self.charsLocation = "../data/characterLists"
        #Stored and extracted data:
        self.characters = {}
        self.allQuotes = []
        #the positions of each unicode point of interest
        self.locations = {
            "2018":[],
            "2019":[],
            "201c":[],
            "201d":[],
            "60":[],
            "22":[],            
            }


        #joined to an offset from above:
        with codecs.open(os.path.join(sourceLocation,self.fileName),'r','utf-8') as f:
            self.blob = TextBlob(f.read())
        #load the characters file
        self.loadCharacters()
        #find the locations of quotes
        self.extractStartPoints()
        #get all the quotes
        self.getAllQuotes()

    #look through every character,
    #recording the positions of particular chars
    def extractStartPoints(self):
        for x,char in enumerate(self.blob):
            if char == START:
                self.locations['2018'].append(x)
            if char == STOP:
                self.locations['2019'].append(x)
            if char == ALTSTART:
                self.locations['201c'].append(x)
            if char == ALTSTOP:
                self.locations['201d'].append(x)
            if char == ASCIISTART:
                self.locations['60'].append(x)
            if char == ASCIIQUOTE:
                self.locations['22'].append(x)
        
    #get a quote, from the start point, assumed to be a
    #u2018, until a u2019 is found.
    #TODO: add better heuristics to extract more quotes
    #nContext: the size n of characters stored for context/
    #co-ref resolution
    def getSentence(self,startArrayIndex,nContext=40):
        #start with the passed in start symbol
        theString = self.blob[startArrayIndex]
        curr = startArrayIndex
        next = curr + 1
        #go until you get an end point

        #while curr < len(self.blob) and next < len(self.blob) and (self.blob[curr] != STOP and (self.blob[next] == " " or self.blob[next] == ",")):

        while next < len(self.blob):
            curr += 1
            next += 1
            theString += self.blob[curr]
            if self.blob[curr] == STOP \
            and (self.blob[next] == " " \
                 or self.blob[next] == "\n" \
                 or self.blob[next] == "," \
                 or self.blob[next] == "."):
                break

        #print "Bounds:",startArrayIndex,curr,(startArrayIndex - nContext),(curr + nContext)
            
        return {
            "string" : theString,
            "start" : startArrayIndex,
            "end" : curr,
            "pre" : self.blob[(startArrayIndex - nContext):startArrayIndex],
            "post" : self.blob[curr:(curr + nContext)]
        }


    #for a starting character, and the index in that list
    #with nContext either side, get a quote
    def simpleQuote(self,char,locationIndex,nContext):
        sent = self.getSentence(self.locations[char][locationIndex],nContext)
        #using object defined on line this-10
        return sent
        
    def easyPrint(self,sentObject):
        print "Pre : ",sentObject['pre']
        print "Sen : ",sentObject['string']
        print "Post: ",sentObject['post']
    
    #get all quotes
    def getAllQuotes(self):
        self.allQuotes = []
        #Find all quotes beginning with u2018
        for x,value in enumerate(self.locations[self.focusValue]):
            self.allQuotes.append(self.getSentence(value))

        print "Found Quotes: " + str(len(self.allQuotes))


    def loadCharacters(self):
        charFileName = self.fileName + "_chars"
        if os.path.isfile(os.path.join(self.charsLocation,charFileName)):
            with codecs.open(os.path.join(self.charsLocation,charFileName),'r','utf-8') as f:
                allLines = f.readlines()
                for x in allLines:
                    char,description = x.split(':')
                    self.characters[char.strip()] = description.strip()
        else:
            print "File Not Found: ",os.path.join(self.charsLocation,charFileName)
                    
    def saveQuotes(self):
        toSaveToFile = {}
        toSaveToFile['characters'] = self.characters
        stringQuotes = []
        for x in self.allQuotes:
            stringQuotes.append({
                "string":unicode(x["string"]),
                "start" :x['start'],
                "end"   :x['end'],
                "pre"   :unicode(x['pre']),
                "post"  :unicode(x['post'])
                })
        toSaveToFile['quotes'] = stringQuotes
        convertedToJson = json.dumps(toSaveToFile,sort_keys=True, indent=4, separators=(',',': '))
        outputName = os.path.join(self.outputLocation,self.outputJsonName)
        print "Saving to:",outputName
        with codecs.open(outputName,"w","utf-8") as f:
            f.write(convertedToJson)

        
if __name__ == "__main__":
    fileName = 'Horus Rising - Dan Abnett.txt'
    focusValue = '2018'
    if len(sys.argv) > 3:
        fileName = sys.argv[1]
        focusValue = svs.argv[2]
        print "Analysing file: ",fileName
        novelAnalysis = Novel_Analysis(fileName,focusValue)
        print "-------"
        novelAnalysis.saveQuotes()
    else:

        potentialFiles = [f for f in os.listdir(sourceLocation) if os.path.isfile(os.path.join(sourceLocation,f))]
        for x in potentialFiles:
            na = Novel_Analysis(x,focusValue)
            na.saveQuotes()
