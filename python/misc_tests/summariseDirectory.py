"""
Script to do a breadth first search of a directory,
    get all pdf files in the tree,
    and output them as an org file tree of links
"""
from collections import namedtuple
from hashlib import md5
from os import listdir, getcwd
from os.path import abspath, join, isfile, isdir
import IPython
import argparse

parser = argparse.ArgumentParser("")
parser.add_argument('-t', '--target', default=getcwd())
parser.add_argument('-o', '--output', default="summary.org")
parser.add_argument('-d', '--depth', default=1)
args = parser.parse_args()

NodeLoc = namedtuple('Nodeloc', 'depth name dict')
Entry = namedtuple('Entry', 'pathlist name')

orgHeading = lambda x: "{} DIR: {}".format(x.depth * '*',x.name)
orgLinkHeading = lambda d,p,t: "{} TODO [[file://{}][{}]]".format((d+1) * '*',p,t)

startDir = args.target
outputFile = args.output
hashset = set()
root_node = {}
#A list of lists to be os.path.join'd
stack = []
#populate the stack:
for loc in [x for x in listdir(startDir) if x != '.git']:
    stack.append(Entry([startDir],loc))

#go down the tree
while len(stack) > 0:
    current = stack.pop()
    fullpath = join(*current.pathlist,current.name)
    pathIsAFile = isfile(fullpath)
    pathIsADir = isdir(fullpath)

    if pathIsADir:
        #directories get all subdirs
        filteredDir = [x for x in listdir(fullpath) if x != '.DS_Store' and x != '.git']
        for newloc in filteredDir:
            stack.append(Entry([*current.pathlist,current.name],newloc))
    elif pathIsAFile:
        #files store themselves in the tree
        tree_position = root_node
        for loc in current.pathlist[1:]:
            if loc not in tree_position:
                tree_position[loc] = {}
            tree_position = tree_position[loc]
        #at the end of the tree is the file itself
        #this should be unique
        assert(current.name not in tree_position)
        tree_position[current.name] = current



#convert to sequence of headings and links to print
sequence = []
depthStack = [NodeLoc(1,'Documents',root_node)]
while len(depthStack) > 0:
    current = depthStack.pop()
    documents = [x for x in current.dict.values() if isinstance(x,Entry)]
    dirs = [NodeLoc(current.depth+1,k,v) for (k,v) in current.dict.items() if isinstance(v,dict)]
    sequence.append(orgHeading(current))
    for doc in documents:
        sequence.append(orgLinkHeading(current.depth,abspath(join(*doc.pathlist,doc.name)),doc.name))

    depthStack.extend(dirs)


#then output to an org file:
with open(join(startDir,outputFile),'a') as f:
    singleText = "\n".join(sequence)
    f.write(singleText)
