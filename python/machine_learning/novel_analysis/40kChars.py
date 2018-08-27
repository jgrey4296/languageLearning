from textblob import TextBlob
import codecs
import re
import sys
import os

##
# Program to find and scan the dramatis personae preface
# of horus heresy novels, extracting character names
# and basic titles
#



#Base directory to save data
base = "/Users/johngrey/github/novel_analysis/data"

#if this is being run as the main program:
if __name__ == "__main__":
    if len(sys.argv) > 1:
        fileName = sys.argv[1]
    else:
        fileName = os.path.join(base,"source_data/Horus Rising - Dan Abnett")
    if len(sys.argv) > 2:
        outFileName = os.path.join(base,"parsed_data",sys.argv[2])
    else:
        outFileName = os.path.join(base,"parsed_data",
                                   fileName + "_chars")

        
print "Using Filename:",fileName
#Regexs
dramatisReg = re.compile(r'DRAMATIS|PERSONAE',re.IGNORECASE)
#capitalReg = re.compile(ur'[\u2018\u2019a-z\( ]{2,15}[\u2019\),\n$]',re.IGNORECASE)
splitReg = re.compile(ur'(\u2018?([A-Z\u2019\'\-]{2,} ?){2,}\u2019?),?([a-zA-Z0-9,\- \.\u2019\u2018]+)?$')


#Open the book
file = codecs.open(fileName,"r","utf-8")

print "Finding Dramatis Personae"
dramatisCount = 0
needed = 1
line = file.readline()
while line != "" and dramatisCount < needed:
    if dramatisReg.search(line):
        dramatisCount += 1
    line = file.readline()

if dramatisCount < needed:
    print "None Found"
    exit()

    
print "Finding Lines describing Chars"
linesToUse = []
#go through each line, untill there have been 5 non-matching lines
failCount = 0
print "STARTING SEARCH AT:",line
while line != "" and failCount < 8:
    if line != "\n":
        if splitReg.match(line):
            linesToUse.append(line)
            failCount = 0
        else:
            failCount += 1
    else:
        print "rejecting:",line
        failCount += 1
    if len(line) > 68:
        failCount += 1
    line = file.readline()

print "Number of Potential Lines:",len(linesToUse)
    
if len(linesToUse) == 0:
    print "No Applicable Lines Found"
    exit()
    

#EXTRACT THE CHARACTERS:
characters = {}
for x in linesToUse:
    match = splitReg.match(x)
    if match:
        print "Adding:",match.group(1)
        characters[match.group(1)] = match.group(3)

print "Found: ", len(characters.keys()),"/",len(linesToUse)
        
# print "Found Characters:",
# for x in characters.keys():
#     print x,characters[x]

#Save to a file:
outFile = codecs.open(outFileName,"w","utf-8")
print "Saving To:",outFileName
for x in characters.keys():
    outFile.write("%s : %s\n" % (x,characters[x]))

outFile.close()
exit()

        
