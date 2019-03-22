
#imports:
from __future__ import print_function
import csv
import numpy
import random
import math

#MEAN
def mean(numberList):
    return sum(numberList)/float(len(numberList))

#VARIANCE
#numberList-1 because we dont count the class tag
def variance(numberList):
    avg = mean(numberList)
    var = sum([pow(x-avg,2) for x in numberList]) \
               / float(len(numberList)-1)
    return var

#STDDEVIATION
def stdev(numberList):
    var = variance(numberList)
    return math.sqrt(var)

#calc probability that 'x' is in the population of mean+stdev
# (1 / sqrt(2*pi*(stdev^2))) * e^(-((x-mean)^2 / 2 * (std^2))
def calculateProbability(x,mean,stdev):
    exponent = math.exp(-(math.pow(x-mean,2) \
                          /(2*math.pow(stdev,2))))
    prob = (1 / (math.sqrt(2*math.pi) * stdev)) * exponent
    return prob

#take the summaries of the classes, and a specific vector to compare against them
def calculateClassProbability(summaries,inputVector):
    #the probabilities inputVector being in each class
    probabilities = {}
    #for all classes
    for classValue, classSummaries in summaries.iteritems():
        #start with a probability of 1
        probabilities[classValue] = 1
        #for all elements in the vector to test
        for i in range(len(classSummaries)):
            mean,stddev = classSummaries[i]
            x = inputVector[i]
            #AND in the probabilities for each element
            probabilities[classValue] *= calculateProbability(x,mean,stddev)
    return probabilities

#given a summary of classes, and an input vector
#predict what class the vector belongs to
def predict(summaries,inputVector):
    probabilities = calculateClassProbability(summaries,inputVector)
    bestLabel, bestProb = None, -1
    #sort for the highest probability, and return the
    #related label
    for classValue, probability in probabilities.iteritems():
        if bestLabel is None or probability > bestProb:
            bestProb = probability
            bestLabel = classValue
    return bestLabel
            
#load the data:
def loadDataset(fname):
    f = open(fname)
    data = list(csv.reader(f))
    f.close()
    print( "Data Length: ",str(len(data)))
    dataset = []
    for i in range(len(data)):
        dataset.append([float(x) for x in data[i]])
    return dataset

def splitData(data,ratio):
    #training data size:
    trainsize = int(len(dataset) * ratio)

    #Split into training and testing data:
    training = []
    testing = list(dataset)

    while len(training) < trainsize:
        idx = random.randrange(len(testing))
        training.append(testing.pop(idx))

    #Training and Testing both setup now
    
    print( "Testing Data: ",len(testing))
    print( "Training Data: ",len(training))
    return [testing,training]

#group data by *known* class:
def separateByClass(data):
    classesWithData = {}
    #for each entry:
    for i in range(len(data)):
        row = data[i]
        #create a label if necessary:
        #-1 is the class tag
        if(row[-1] not in classesWithData):
            classesWithData[row[-1]] = []
        #add to that label:
        classesWithData[row[-1]].append(row)
    return classesWithData

#Summarise data with mean and stddeviation for classes:
#takes an array of arrays of numbers
#zip(*data) collects each element from each array and combines
#so zip(*[[a,b,c][d,e,f]]) = [(a,d),(b,e),(c,f)]
def summarize(data):
    summaries = [(mean(attribute),stdev(attribute)) \
                 for attribute in zip(*data)]
    #get rid of the summary of the class tag, its meaningless
    del summaries[-1]
    return summaries

def summarizeByClass(data):
    separated = separateByClass(data)
    summaries = {}
    #Number and array of arrays of numbers
    for classVal,instances in separated.iteritems():
        summaries[classVal] = summarize(instances)
    return summaries


def getPredictions(summaries,testSet):
    predictions = []
    for i in range(len(testSet)):
        result = predict(summaries,testSet[i])
        predictions.append(result)
    return predictions

def getAccuracy(testSet,predictions):
    correct = 0
    for x in range(len(testSet)):
        if testSet[x][-1] == predictions[x]:
            correct += 1
    return (correct/float(len(testSet))) * 100.0


if __name__ == "__main__":
    print("Naive Bayes Testing")
    dataset = loadDataset("./data/pima.csv")
    testing,training = splitData(dataset,0.02)
    print("TestSet:",testing[0])
    print("Training:",training[0])
    summaries = summarizeByClass(training)
    print("Summaries: ",summaries.keys())
    print("Summary 1: ",summaries[0])
    print("Summary 2: ",summaries[1])
    predictions = getPredictions(summaries,testing)
    print("Predictions: ",predictions[0:10])
    accuracy = getAccuracy(testing,predictions)
    print("Accuracy: ",accuracy)

