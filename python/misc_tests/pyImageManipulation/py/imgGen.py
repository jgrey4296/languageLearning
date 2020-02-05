#!/Users/jgrey/anaconda/bin/python
import IPython
from PIL import Image
import numpy as np
import numpy.random as rnd
import os.path as path
import pylab

size = (100,100,4)

arr = rnd.random(size) + np.array([0,0,0,255])
arr *= np.array([255,255,255,1])


im = Image.fromarray(np.uint8(arr))

pylab.imshow(im)
pylab.show()
