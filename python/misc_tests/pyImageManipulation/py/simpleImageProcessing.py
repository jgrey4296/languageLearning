#!/Users/jgrey/anaconda/bin/python
import IPython
from PIL import Image
import numpy as np
import os.path as path
import pylab

IMAGE_DIR = "imgs"
IMAGE_TO_LOAD = "street_spirit.png"

im = Image.open(path.join(IMAGE_DIR,IMAGE_TO_LOAD))
box = (25,25,100,100)
region = im.crop(box)
region = region.transpose(Image.ROTATE_180)
im.paste(region,box)

imArr = np.array(im)

arrIm = Image.fromarray(imArr)

pylab.imshow(arrIm)
pylab.show()

