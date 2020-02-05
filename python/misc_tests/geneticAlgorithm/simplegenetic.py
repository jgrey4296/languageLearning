#based on outlate.com/Simple-Genetic-Algorithm-in-15-lines-of-Python/

import random as rn, numpy as np
import math
import IPython

#init sizes:
initial_population = 100
mutation_rate = 0.1
number_of_generations = 10
dna_sequence_size = 6
dna_sequence_split = dna_sequence_size // 2
survivors_size = 10
#create the population:
curPop = np.random.random((initial_population,dna_sequence_size))
nextPop = np.zeros((curPop.shape[0],curPop.shape[1]))
fitness_vector = np.zeros((initial_population,2))

#The cost function:
def costFunction(datum):
    v1 = datum[:dna_sequence_split]
    v2 = datum[dna_sequence_split:]
    dot = np.dot(v1,v2)
    return sigmoid(dot)

def sigmoid(x):
    return 1 / (1 + np.exp(-x))

#------------------------------

#run all generations:
for i in range(number_of_generations):
    print("Running Generation:",i, curPop.shape, curPop[0].shape)
    #calc fitnesses
    fitness_vector = np.array([np.array([x,costFunction(curPop[x])]) for x in range(initial_population)])
    avgError = np.sum(fitness_vector[:,1]) / initial_population
    print('Avg Error: ',avgError)

    #select winner batches:
    survivors = np.zeros((survivors_size,dna_sequence_size))
    for n in range(len(survivors)):
        #get a selection of individuals
        selected = np.random.choice(range(len(fitness_vector)), survivors_size//2,replace=False)
        #select the survivor
        survivor_i = np.argmin(fitness_vector[selected,1])
        #retrieve the survivors's dna
        survivors[n] = curPop[int(fitness_vector[selected[survivor_i]][0])]

    #copy winners to the new population:
    nextPop[:len(survivors)] = survivors
    #mate winners to produce the full population:
    duplicWin = np.zeros(((initial_population - len(survivors)),survivors.shape[1]))

    #mate, by dna entry one at a time
    for x in range(survivors.shape[1]):
        numDups = (initial_population - len(survivors))/len(survivors)
        duplicWin[:,x] = np.repeat(survivors[:,x], numDups, axis=0)
        duplicWin[:,x] = np.random.permutation(duplicWin[:,x])

    #copy over the mated data
    nextPop[len(survivors):] = np.matrix(duplicWin)
    #mutate:
    mutMatrix = np.matrix([np.float(np.random.normal(0,2,1)) if rn.random() < mutation_rate else 1 for x in range(nextPop.size)]).reshape(nextPop.shape)
    nextPop = np.array(np.multiply(nextPop,mutMatrix))
    curPop = nextPop

    #output Stats:
    current_best = np.argmin(fitness_vector[:,1])
    print("Current Best:",fitness_vector[current_best][1] ,curPop[current_best])
    print("\n")
    #end of generations

#show the results:
print('best:',curPop[np.argmin(fitness_vector[:,1])])
