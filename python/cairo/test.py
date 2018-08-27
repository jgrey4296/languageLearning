"""
    Simple Sine manipulation
"""
# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.cairosine"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################
# IMPORTS
####################
import cairo
import numpy as np
import cairo_utils as utils
from math import pi
from os.path import join,isfile,exists,isdir
from os import listdir
import argparse
import IPython

from cairo_utils import Parabola, Quadratic

##############################
# CONSTANTS
####################
N = 10
X = pow(2,N)
Y = pow(2,N)
surface, ctx, size, N = utils.drawing.setup_cairo(N=N, scale=False, cartesian=True)
FILENAME = "test"
IMAGE_DIR = "images"

SAMPLENUM = 1000
RADIUS = 0.01
NUM_OF_FRAMES = 100
##############################
# VARIABLES
####################
base_indices = np.linspace(0,2 * pi,SAMPLENUM)
base_sine = np.sin(base_indices)

fast_sine = np.linspace(0,2*pi, int(SAMPLENUM * 0.5))

#utility modifiers
ss = lambda s, x : s * x
os = lambda s, x : s + x
sos = lambda s, x : (s * x) + x


##############################
# Utilities
####################
def draw(i=0,m=1):
    """ Draw a single image of a sine wave.
    i : index of the image to write
    m : scaling factor of the image
    """
    utils.drawing.clear_canvas(ctx, bbox=np.array([0,0,size,size]))
    ctx.set_source_rgba(*[1,0,0,1])

    parabola = Parabola(size * 0.2, size * 0.23, size * 0.2)
    parabola.update_d(size * 0.2)
    xs = np.linspace(0, size, 4000)
    xys = parabola(xs)

    for x,y in xys:
        utils.drawing.drawCircle(ctx, x,y,3)

    ctx.set_source_rgba(*[0,1,0,1])
    p2 = Parabola(size * 0.7, size * 0.21, size * 0.2)
    p2.update_d(size * 0.2)
    xys2 = p2(xs)

    for x,y in xys2:
        utils.drawing.drawCircle(ctx, x,y,3)

    ctx.set_source_rgba(*[0,0,1,1])
    i_xys = parabola.intersect(p2)
    IPython.embed(simple_prompt=True)
    print(len(i_xys))
    for x,y in i_xys:
        utils.drawing.drawCircle(ctx, x,y, 7)

        
    utils.drawing.write_to_png(surface,join(IMAGE_DIR,FILENAME),i)

def draw_multiple():
    """ Draw NUM_OF_FRAMES images, scaling from 0 to 1 """
    frameNum = range(NUM_OF_FRAMES)
    base_num = list(range(int(NUM_OF_FRAMES * 0.5)))
    rev_num = base_num.copy()
    rev_num.reverse()
    combined = base_num + rev_num
    
    for i,m in zip(frameNum, soft_knee(combined,1.0,2.0,0.3)):
        logging.info("Drawing Frame: {}".format(i))
        draw(i,(m/(NUM_OF_FRAMES*0.2)))


#An example shaping function
def shape(x): return (4/9 * pow(x,6)) - (17/9 * pow(x,4)) + (22/9 * pow(x,2))


# A Simple soft_knee compression curve
# Source: https://se.mathworks.com/help/audio/ref/compressor-class.html
# Input: 1d np.array(), Threshold, Ratio, Knee width
def soft_knee(i,t,r,k):
    under = np.array([x for x in i if x < (t - k/2)])
    inKnee = np.array([x for x in i if (t - k/2) <= x and x <= (t + k/2)])
    over = np.array([x for x in i if (t + k/2) < x])

    k_f = (1/r - 1)
    intermediate = inKnee - t + k/2
    intermediate_pow = pow(intermediate,2)
    k_div_amnt = 2 * k
    k_mod = (k_f * intermediate_pow) / k_div_amnt
    k_red = inKnee + k_mod
    
    over_red = t + ((over - t)) / r

    return np.concatenate((under,k_red,over_red))



##############################
# Core Functions
####################


#Argparse setup:
ap = argparse.ArgumentParser()
ap.add_argument('-s','--single',help="Render just a single image",
                action="store_true")
          
########################################
if __name__ == "__main__":
    logging.info("Starting ")
    args = ap.parse_args()
    if args.single:
        draw(0,1)
    else:
        draw_multiple()
