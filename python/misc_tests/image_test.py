# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
logFileName = "image_test.log"
root_logger.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)

import PIL
from PIL import Image
from random import random

im = Image.open('data/overworld.png')
xs,ys = im.size
minx = xs * 0.05
miny = xs * 0.05

logging.info("Size: {} - {}".format(xs,ys))

def rbox(oldbox=None):
    if oldbox:
        xpos = (random() * (xs - oldbox[2]))
        ypos = (random() * (ys - oldbox[3]))
        xdelta = xpos + (oldbox[2] - oldbox[0])
        ydelta = ypos + (oldbox[3] - oldbox[1])
    else:
        xpos = (random() * (xs - minx))
        ypos = (random() * (ys - miny))
        xdelta = xpos + (random() * (xs - xpos))
        ydelta = ypos + random() * (ys - ypos)
    return (int(xpos),int(ypos),int(xdelta),int(ydelta))

for i in range(50):
    box = rbox()
    logging.info("Box: {}".format(box))
    region = im.crop(box)
    newbox = rbox(box)
    logging.info("New Box: {}".format(newbox))
    #region = region.transpose(Image.ROTATE_180)
    im.paste(region, newbox)


im.save('output.png')
