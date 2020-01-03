import pyparsing as pp
import networkx as nx

#Parsers:
#require statement
#function definition
#class definition
#handlers
#states
#actions
#ingredients



def parse_lua_file(filename, name_parsers, contents_parsers):
    """ Take a filename, open it,
    read its lines, and extract
    function information"""
    lines = []
    with open(filename,'r') as f:
        lines = f.readlines()

    function_names = []

    current_name = ""
    current_mentions = []

    function_dict = {}
    #initial pass to get names:
    for line in lines:
        continue

    #second pass to get contents:
    for line in lines:
        continue

    #Combine into graph
    graph = nx.graph()

    return grap
