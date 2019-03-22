import networkx as nx
# Setup logging:
import logging
import graphviz
LOGLEVEL = logging.DEBUG
logFileName = "networkX_test.log"
logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')

console = logging.StreamHandler()
console.setLevel(logging.INFO)
logging.getLogger('').addHandler(console)

#Graph, DiGraph...
G = nx.Graph()

#Add nodes:
G.add_node(1)
#add_node, add_nodes_from,
#add_path, add_star, add_cycle

#add_edge, add_edges_from,
#has_edge, has_node,

#.nodes(), .edges(), .neighbours() 

#nx.connected_components(G)
#nx.degree(G).values()
#nx.clustering(G)

#Check graphviz.ENGINES
G = graphviz.Digraph(engine='fdp')
G.node(NAME,LABEL)
G.edge(TAIL,HEAD)
G.render(FILENAME,view=True)


import matplotlib.pyplot as plt

G = nx.petersen_graph()
plt.subplot(121)

nx.draw(G, with_labels=True, font_weight='bold')
plt.subplot(122)

nx.draw_shell(G, nlist=[range(5, 10), range(5)], with_labels=True, font_weight='bold')

import networkx as nx
import pygraphviz as pgv # need pygraphviz or pydot for nx.to_agraph()

G = nx.DiGraph()
G.add_edge(1,2,weight=7)
G.add_edge(2,3,weight=8)
G.add_edge(3,4,weight=1)
G.add_edge(4,1,weight=11)
G.add_edge(1,3)
G.add_edge(2,4)

for u,v,d in G.edges(data=True):
    d['label'] = d.get('weight','')

A = nx.to_agraph(G)
A.layout(prog='dot')
A.draw('test.png')
