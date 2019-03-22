"""
    Kmeans graph tutorial
http://www.learndatasci.com/k-means-clustering-algorithms-python-intro/?utm_content=buffer66b41&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer
"""
# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "kmeansgraph.log"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################
# IMPORTS
####################
import networkx as nx
import graphviz
import numpy as np
import pandas
import statsmodels
import scipy
import seaborn
import sklearn
from sklearn import cluster
from sklearn import metrics
from collections import defaultdict

##############################
# CONSTANTS
####################
G = nx.karate_club_graph()
groundTruth = [0,0,0,0,0,
               0,0,0,1,1,
               0,0,0,0,1,
               1,0,0,1,0,
               1,0,1,1,1,
               1,1,1,1,1,
               1,1,1,1]

KCLUSTERS = 2
##############################
# VARIABLES
####################

##############################
# Utilities
####################
def graphToEdgeMatrix(g):
    return nx.to_numpy_matrix(g)

def drawCommunities(graph,partition,pos):
    g = graphviz.Graph(engine='fdp')
    numColours = len(set(partition))
    colours = [arrToHexColour(np.random.random(3)) for i in range(numColours)]

    for i,(node,part) in enumerate(zip(graph.nodes(),partition)):
        g.node(str(node),style="filled",fillcolor=colours[part])

    for tail,head in graph.edges():
        g.edge(str(tail),str(head))

    g.render('test.png',view=True)

def arrToHexColour(arr):
    """ Convert a triple of 0.0-1.0 values to hex """
    to255 = [int(255*x) for x in arr]
    toStrings = [format(x,'02x') for x in to255]
    return "#{}".format("".join(toStrings))


#CLUSTERING;
def cluster_kmeans(graph):
    kmeans = cluster.KMeans(n_clusters=KCLUSTERS, n_init=100)
    kmeans.fit(nx.to_numpy_matrix(graph))
    return list(kmeans.labels_)

def cluster_agglomerative(graph):
    agglo = cluster.AgglomerativeClustering(n_clusters=KCLUSTERS,linkage='ward')
    agglo.fit(nx.to_numpy_matrix(graph))
    return list(agglo.labels_)

def cluster_spectral(graph):
    spectral = cluster.SpectralClustering(n_clusters=KCLUSTERS,
                                          affinity="precomputed",n_init=200)
    spectral.fit(nx.to_numpy_matrix(graph))
    return list(spectral.labels_)

def cluster_affinity(graph):
    affinity = cluster.affinity_propagation(S=nx.to_numpy_matrix(graph),
                                            max_iter=200, damping=0.6)
    return list(affinity[1])

def compute_metrics(graph):
    results = []
    results.append(('kmeans',cluster_kmeans(G)))
    results.append(('agglo',cluster_agglomerative(G)))
    results.append(('spectral',cluster_spectral(G)))
    results.append(('affinity',cluster_affinity(G)))

    metric_results = [(name,
                metrics.normalized_mutual_info_score(groundTruth,x),
                metrics.adjusted_rand_score(groundTruth,x)) for name,x in results]

    return metric_results




##############################
# Core Functions
####################

########################################
if __name__ == "__main__":
    logging.info("Starting kmeansgraph")
