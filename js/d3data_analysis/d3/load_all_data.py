"""
The main access script, using the other scripts
"""
import numpy as np
import pygraphviz as pgv
import IPython
# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
logFileName = "log.all_data_load"
root_logger.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)

import matplotlib.pyplot as plt
from collections import defaultdict
import re
import networkx as nx
import csv_loading as csvl
import d3_data_classes as d3dc


#load all the data
policies = [x for x in csvl.load_d3_csv('policies.csv',lambda i,x: d3dc.make_policy(i, x[1:])) if x is not None]
policyGroups = [x for x in csvl.load_d3_csv('policygroups.csv',lambda i,x:d3dc.PolicyGroup(x[1])) if x is not None]
pressureGroups = [x for x in csvl.load_d3_csv('pressuregroups.csv',lambda i,x: d3dc.make_pressure_group(i, x)) if x is not None]
simStates = [x for x in csvl.load_d3_csv('simulation.csv',lambda i,x: d3dc.make_simstate(i, x)) if x is not None]
situations = [x for x in csvl.load_d3_csv('situations.csv',lambda i,x: d3dc.make_situation(i, x)) if x is not None]
sliders = [x for x in csvl.load_d3_csv('sliders.csv',lambda i,x: d3dc.make_slider(i, x)) if x is not None]
voters = [x for x in csvl.load_d3_csv('votertypes.csv',lambda i,x: d3dc.make_voter(i, x)) if x is not None]
#dictionaries
policiesD = { x.id_str : x for x in policies }
policyGroupD = { x.id_str : x for x in policyGroups }
pressureGroupD = { x.id_str : x for x in pressureGroups }
simStateD = { x.id_str : x for x in simStates }
situationD = { x.id_str : x for x in situations }
sliderD = { x.id_str : x for x in sliders }
voterD = { x.id_str : x for x in voters }

G = nx.MultiDiGraph()

allStructs = {}
allStructs.update(policiesD)
allStructs.update(policyGroupD)
allStructs.update(pressureGroupD)
allStructs.update(simStateD)
allStructs.update(situationD)
allStructs.update(sliderD)
allStructs.update(voterD)

#Add all ndoes to the graph
#The graph is just the id's, all data is held in allStructs
G.add_nodes_from(allStructs.keys())

#Add all links:
for x in allStructs.values():
    if hasattr(x, 'link'):
        x.link(G)

def mag(G):
    """ Convert a graph to an agraph for use with pygraphviz """
    A = nx.drawing.nx_agraph.to_agraph(G)
    return A

def draw_graph(G, name, prog='dot'):
    #['neato'|'dot'|'twopi'|'circo'|'fdp'|'nop']
    A = nx.drawing.nx_agraph.to_agraph(G)
    A.layout(prog=prog)
    A.draw('{}.png'.format(name))


def filter_graph_for(g, target):
    passing_edges = [x for x in g.edges() if target in x]
    passing_nodes = set([x for a in passing_edges for x in a])
    newsubgraph = nx.Graph(g.subgraph(passing_nodes))
    for x,y in list(newsubgraph.edges()):
        if x != target and y != target:
            newsubgraph.remove_edge(x,y)
    return newsubgraph


gdp_graph = filter_graph_for(G, 'GDP')
gdpa = mag(gdp_graph)

#counting:
counts = defaultdict(lambda: 0)
for x in policies:
    counts[x.policyGroup] += 1
counts = dict(counts)


def barplot(data, ylabel, xlabel, title, reverse=False, sort_by_value=True):
    assert(isinstance(data, dict))
    data_l = list(data.items())
    if sort_by_value:
        data_l.sort(key=lambda x: x[1])
    else:
        data_l.sort(key=lambda x: x[0])
    if reverse:
        data_l.reverse()
    pos = np.arange(len(data_l))
    
    figure, axes = plt.subplots()
    axes.bar(pos, [x[1] for x in data_l], align='center')
    axes.set_xticks(pos)
    axes.set_xticklabels([x[0] for x in data_l])
    axes.set_ylabel(ylabel)
    axes.set_xlabel(xlabel)
    axes.set_title(title)

    return (figure, axes)

#From: https://stackoverflow.com/questions/31729948/
def reshow_figure(fig):
    dummy = plt.figure()
    new_manager = dummy.canvas.manager
    new_manager.canvas.figure = fig
    fig.set_canvas(new_manager.canvas)


logging.info("Finished parsing")
IPython.embed(simple_prompt=True)


