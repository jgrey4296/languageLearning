"""
An experiment in generation by jigsaw
"""
# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = ".log"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################
# IMPORTS
####################
import IPython
import numpy as np
import numpy.random as nprand
import random
import cairo
import utils
from enum import Enum
N = 10
X = pow(2,N)
Y = pow(2,N)
surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, X,Y)
ctx = cairo.Context(surface)
ctx.scale(X,Y) #coords in 0-1 range


BOARD_SIZE = 3


Direction = Enum('Direction','LEFT RIGHT UP DOWN DIAG')

def opposite_direction(e):
    if e is Direction.LEFT:
        return Direction.RIGHT
    elif e is Direction.RIGHT:
        return Direction.LEFT
    elif e is Direction.UP:
        return Direction.DOWN
    else:
        return Direction.UP

class JPiece:
    """ An individual Jigsaw piece """
    row_length = 3
    
    def __init__(self,i):
        self.i = i
        #Colours are a Direction : (colour, Locked?)
        self.colours = {
            'main'         :  [np.concatenate((nprand.random(3),[1])), False],
            Direction.LEFT :  [np.concatenate((nprand.random(3),[1])), False],
            Direction.RIGHT:  [np.concatenate((nprand.random(3),[1])), False],
            Direction.UP   :  [np.concatenate((nprand.random(3),[1])), False],
            Direction.DOWN :  [np.concatenate((nprand.random(3),[1])), False]
        }

    def get_adjacency_coords(self):
        x,y = self.get_coords()
        adj = [(x+i, y+j) for i,j in zip([-1,0,1,0],[0,1,0,-1])]
        return adj

        
    def fixup(self,f=False,adj=None):
        if f:
            logging.info("Setting all colours of {}".format(self.i))
            for key in self.colours.keys():
                self.colours[key][1] = True            
        if adj is not None:
            logging.info("Setting borders with {}".format([x.i for x in adj]))
            #go through each edge and fix it
            for piece in adj:
                direction = self.get_relation_direction(piece)
                o_dir = opposite_direction(direction)
                if not all(self.colours[direction][0] == piece.colours[o_dir][0]):
                    logging.info("Fixing Border: {} ({},{})".format(direction,*piece.get_coords()))
                    piece.colours[o_dir] = self.colours[direction].copy()
                
                self.colours[direction][1] = True
                logging.info("Modifing {} {}".format(piece.i, o_dir))
                piece.colours[o_dir][1] = True


        logging.info("Setting Edges of board")
        x,y = self.get_coords()
        adj = self.get_adjacency_coords()
        with_direction = list(zip(adj,[Direction.LEFT,Direction.DOWN,Direction.RIGHT,Direction.UP]))
        out_of_bounds = list(filter(lambda t: 0 > t[0][0] or t[0][0] >= self.row_length or 0 > t[0][1] or t[0][1] >= self.row_length, with_direction))
        for coords,direction in out_of_bounds:
            logging.info("Blacking {}".format(direction))
            self.colours[direction] = [np.array([0,0,0,1]), True]

            
 
    def get_relation_direction(self,other):
        x,y = self.get_coords()
        x2,y2 = other.get_coords()
        horizontal = x-1 == x2 or x+1 == x2
        vertical = y-1 == y2 or y+1 == y2
        if horizontal and vertical:
            return Direction.DIAG
        elif horizontal:
            if x-1 == x2:
                return Direction.LEFT
            else:
                return Direction.RIGHT
        else:
            if y-1 == y2:
                return Direction.UP
            else:
                return Direction.DOWN
                    
            
    def __eq__(self,other):
        if self.i == other.i:
            return True
        direction = self.get_relation_direction(other)
        o_dir = opposite_direction(direction)
        return self.colours[direction] == self.colours[o_dir]
    
    def get_coords(self):
        return (self.i%self.row_length, int(self.i/self.row_length))
        
    def to_bounds(self):
        s = 1 / self.row_length
        x,y = self.get_coords()
        as_tuple =  (s*x, s*y, s,s)
        logging.info("{} Bounds : {:.2f}-{:.2f} / {:.2f}-{:.2f}".format(self.i, *as_tuple))
        return as_tuple
        
    def draw(self):
        x,y,xd,yd = self.to_bounds()
        ctx.set_source_rgba(*self.colours['main'][0])
        utils.drawRect(ctx,x,y,xd,yd)

        centre = np.array([x + xd*0.5, y + yd*0.5])
        left = centre - [xd*0.2,0]
        right = centre + [xd*0.2,0]
        up = centre - [0,yd*0.2]
        down = centre + [0,yd*0.2]
        r = 1 / (16*2)

        ctx.set_source_rgba(*self.colours[Direction.LEFT][0])
        utils.drawCircle(ctx,*left,r)
        ctx.set_source_rgba(*self.colours[Direction.RIGHT][0])
        utils.drawCircle(ctx,*right,r)
        ctx.set_source_rgba(*self.colours[Direction.UP][0])
        utils.drawCircle(ctx,*up,r)
        ctx.set_source_rgba(*self.colours[Direction.DOWN][0])
        utils.drawCircle(ctx,*down,r)

        

class Jigsaw:

    def __init__(self,size):
        self.size = size
        self.pieces = np.arange(0,size*size).reshape((size,size))
        self.piece_objects = []
        JPiece.row_length = size
        for i in self.pieces.flatten():
            self.piece_objects.append(JPiece(i))

    def get_adjacent_from_index(self,i):
        adj = self.piece_objects[i].get_adjacency_coords()
        filtered = list(filter(lambda t: 0 <= t[0] < self.size and 0 <= t[1] < self.size, adj))
        back_to_indices = [x + (y*self.size) for x,y in filtered]
        return back_to_indices
    
    def dfs(self):
        stack = [0]
        found = set([])
        while len(stack) > 0:
            curr = stack.pop()
            logging.info("Curr: {}".format(curr))
            if curr in found:
                continue
            adj = self.get_adjacent_from_index(curr)
            self.piece_objects[curr].colours['main'] = [[0,0,0,1], True]
            self.piece_objects[curr].fixup(f=curr==0,adj=[self.piece_objects[i] for i in adj])
            found.add(curr)
            stack.extend(adj)

                        
    def draw(self):
        for x in self.piece_objects:
            x.draw()
            
########################################
if __name__ == "__main__":
    logging.info("Starting ")
    jigsaw = Jigsaw(BOARD_SIZE)
    jigsaw.dfs()
    jigsaw.draw()
    utils.write_to_png(surface,"test")
