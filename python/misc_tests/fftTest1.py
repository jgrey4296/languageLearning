from scipy.io import wavfile
from scipy.fftpack import fft, ifft
import numpy
import random

#load the file
fs, data = wavfile.read('../data/Piano 1.wav')

#fft it in blocks
blockSize = 1024

def modifyChannel(data):
    toReturn = []
    numOfBlocks = int(data.shape[0] / blockSize)
    for x in xrange(numOfBlocks):
        #get a selection
        currentBlock = data[(x*blockSize):((x+1)*blockSize)]
        #fft it
        fftBlock = fft(currentBlock)
        if len(fftBlock) != 1024:
            raise Exception("Block Length difference")

        #modify the block

        for y in xrange(len(fftBlock)):
            fftBlock[y] *= random.uniform(-1.0,1.0)



        #ifft each block
        reconstructedBlock = ifft(fftBlock)
        #cast it back to int16
        castBlock = reconstructedBlock.astype(numpy.int16)

        #add the block to the channel:
        toReturn = numpy.concatenate((toReturn,castBlock))


    if len(toReturn) != data.shape[0]:
        remaining = data.shape[0] - len(toReturn)
        zeros = numpy.zeros(remaining,dtype=numpy.int16)
        toReturn = numpy.concatenate((toReturn,zeros))

    return toReturn

#save
data.T[0] = modifyChannel(data.T[0])
data.T[1] = modifyChannel(data.T[1])
wavfile.write('../data/test.wav',fs,data)
