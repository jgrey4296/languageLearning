# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.overlay"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################
import IPython
from PIL import Image
import numpy as np
from os.path import join, isfile, exists, isdir, splitext
from os import listdir
from random import choice, randrange, random
#Notes:
#box = (left, up, right, low)
#Image.new(mode, size)
#im.resize, im.rotate, im.crop, im.paste
#im.point(lambda)  : modifies each point in the image
#Image.merge(mode, sourcebands)

##### DATA:
OUTPUT_DIR = 'output'
INPUT_DIR = 'imgs'
NEW_SIZE = (400, 400)

#LOADING:
#r/w/rb/wb
def load_images():
    potential_imgs = [x for x in listdir(join('.', INPUT_DIR)) if splitext(x)[1] == '.jpg']
    loaded = {}
    for x in potential_imgs:
        name, type = splitext(x)
        loaded[name] = Image.open(join(INPUT_DIR, x))
        logging.info("Loaded : {}: {} {}".format(name, loaded[name].size, loaded[name].mode))
    return loaded

#Saving:
def save_image(im, name):
    im.save(join('output', name))

#cropping:
def crop_image(im, box=None):
    if box is None:
        box = (0, 0, im.size[0], im.size[1])
    region = im.crop(box)
    return region

def paste_image(im, region, box=None):
    #actually alpha_composite to allow alpha to work
    if box is None:
        box = (0,0, region.size[0], region.size[1])
    blank = Image.new('RGBA', im.size)
    blank.putalpha(0)
    blank.paste(region, box)
    output = Image.alpha_composite(im, blank)
    return output


def fitting_box(total, selection):
    range_x = (0, total[0] - selection[0])
    range_y = (0, total[1] - selection[1])
    try:
        if range_x[1] > 0:
            l = randrange(*range_x)
        else:
            l = 0
        if range_y[1] > 0:
            u = randrange(*range_y)
        else:
            u = 0
        r,d = (l+selection[0], u+selection[1])
    except:
        logging.info("Fitting box")
        IPython.embed(simple_prompt=True)
    return (l, u, r, d)

def stutter(total, im, start=None, stutter_region=None,
            direction="y", repeat_period=5, repeat_num=20):
    """ Take a region from the image, 
      repeat it in a direction, every rp pixels, rn times
    """
    output = total.copy()
    cropped = crop_image(im, stutter_region)
    if start is None:
        start = (0, 0, cropped.size[0], cropped.size[1])
    loc = start
    xplus = 0
    yplus = 0
    if 'x' in direction:
        xplus += repeat_period
    if 'y' in direction:
        yplus += repeat_period
        
    for x in range(repeat_num):
        if loc[0] >= 0 and loc[2] < output.size[0] and \
           loc[1] >= 0 and loc[3] < output.size[1]:
            output = paste_image(output, cropped, loc)
            loc = (loc[0] + xplus, loc[1] + yplus,
                   loc[2]+xplus, loc[3]+yplus)
        else:
            logging.info("Stutter else")
            #IPython.embed(simple_prompt=True)
            break
    return output


def band_map(im, lr=None, lg=None, lb=None, la=None):
    r,g,b,a = im.split()
    if lr is not None:
        r = r.point(lr)
    if lg is not None:
        g = g.point(lg)
    if lb is not None:
        b = b.point(lb)
    if la is not None:
        a = a.point(la)
    merged = Image.merge(im.mode, (r, g, b, a))
    assert merged != im
    return merged



if __name__ == "__main__":
    images = load_images()
    new_image = Image.new('RGBA', NEW_SIZE)
    logging.info("Total: {}".format(new_image))
    new_image = images['Test1'].resize(NEW_SIZE)

    
    for x in range(5):
        im = crop_image(choice(list(images.values())))
        im = im.resize((32, 32))
        im.putalpha(255)
        newbox = fitting_box(new_image.size, im.size)
        new_image = paste_image(new_image, im, newbox)

    for x in range(20):
        stutter_region = fitting_box(new_image.size, (1,64))
        region_size = (stutter_region[2]-stutter_region[0],
                       stutter_region[3] - stutter_region[1])
        newbox = fitting_box(new_image.size, region_size)
        new_image = stutter(new_image, new_image , direction="x",
                            repeat_period=1, repeat_num=randrange(10,30),
                            start=newbox, stutter_region=stutter_region)
        

    save_image(new_image, 'result.jpg')
    new_image.show()
