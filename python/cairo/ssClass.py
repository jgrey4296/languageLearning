import numpy as np
from numpy.random import random
import cairo_utils as utils
import IPython

TWOPI = np.pi*2
HPI = np.pi * 0.5
sampleRange = [10,200]
radiusRange = [0.2,0.35]
interpolationPoints = 3000
p_r = 0.0004 #an individual point radius
noiseAmnt = 0.9
grains = 40
grainMult = 1.2
smooth = 0.9
ALPHA = 0.09
rMod = 1.3

#replicating inconvergents sand spline
class SandSpline(object):

    def __init__(self,ctx,sizeTuple):
        self.ctx = ctx
        self.sX = sizeTuple[0]
        self.sY = sizeTuple[1]

        self.interpolationPoints = interpolationPoints
        self.sampleRange = sampleRange
        self.smooth = smooth
        
        #interation counter:
        self.itt = 0
        #resolution:
        self.resolution = sizeTuple[0]
        #reciprocal resolution:
        self.recRes = 1.0 / sizeTuple[0]
        #noise variance
        self.noise_stp = noiseAmnt
        #tracker of raw xy points:
        self.xys = []
        #tracker of output interpolated points:
        self.i_xys = []
        #tracker of noise points:
        self.noise = []
        #tracker of sample amounts:
        self.snums = []
        #calculated grains:
        self.calculatedGrains = []
        
    #------------------------------
    # SHAPES
    #------------------------------

    def addLine(self,x,y,x2,y2):
        #get a set of random points
        sampleSize = (self.sampleRange[0] + (random(1) * self.sampleRange[1])).astype('int')[0]
        #sort into ascending order
        lineX = sorted(x + (random(sampleSize) * (x2 - x)))
        lineY = sorted(y + (random(sampleSize) * (y2 - y)))
        #combin into a single matrix of size (n,2)
        line = np.column_stack((lineX,lineY))
        #smooth the points
        i2_line = utils.math._interpolate(line,self.interpolationPoints,smoothing=self.smooth)
        initialNoise = np.zeros((sampleSize,1),'float')
        self.snums.append(sampleSize)
        #store the original points, the interpolated points, and the initial noise amount
        self.xys.append(line)
        self.i_xys.append(i2_line)
        self.noise.append(initialNoise)

    def addBezier(self,start,cp1,end):
        sampleSize = (self.sampleRange[0] + (random(1) * self.sampleRange[1])).astype('int')[0]
        #create the bezier line
        line = utils.bezier1cp(*start,*cp1,*end,sampleSize)
        i2_line = utils.math._interpolate(line,self.interpolationPoints,smoothing=self.smooth)
        initialNoise = np.zeros((sampleSize,1),'float')
        self.snums.append(sampleSize)
        self.xys.append(line)
        self.i_xys.append(i2_line)
        self.noise.append(initialNoise)

    def addBezier2cp(self,start,cp1,cp2,end):
        sampleSize = (self.sampleRange[0] + (random(1) * self.sampleRange[1])).astype('int')[0]
        #create the bezier line
        line = utils.math.bezier2cp(start,cp1,cp2,end,sampleSize)
        i2_line = utils.math._interpolate(line,self.interpolationPoints,smoothing=self.smooth)
        initialNoise = np.zeros((sampleSize,1),'float')
        self.snums.append(sampleSize)
        self.xys.append(line)
        self.i_xys.append(i2_line)
        self.noise.append(initialNoise)

    
    def addCircle(self,x=0.5,y=0.5,rLower=radiusRange[0],rHigher=radiusRange[1]):
        sampleSize = (self.sampleRange[0] + (random(1) * self.sampleRange[1])).astype('int')[0]
        randPoints = sorted(random(sampleSize)*TWOPI)
        #create the circle:
        xPoints = x + np.cos(randPoints) * (rLower + random(1) * rHigher)
        yPoints = y + np.sin(randPoints) * (rLower + random(1) * rHigher)
        #combine together:
        circlePoints = np.column_stack((xPoints,yPoints))
        interpolatedPoints = utils.math._interpolate(circlePoints,self.interpolationPoints,smoothing=self.smooth)
        initialNoise = np.zeros((sampleSize,1),'float')
        #store the generated info:
        self.snums.append(sampleSize)
        self.xys.append(circlePoints)
        self.i_xys.append(interpolatedPoints)
        self.noise.append(initialNoise)

        
    #------------------------------
    def draw(self,interpolate,interpolateGrains):
        print('drawing')
        lxys = len(self.i_xys)
        lgrains = len(self.calculatedGrains)
        #for all the xy pairs of the interpolated data
        for i,xy in enumerate(self.i_xys):
            color = [x for x in np.random.random(3)]
            self.ctx.set_source_rgba(*color,ALPHA)
            points = xy

            if interpolate:
                #further interpolate the points:
                print('interpolating',i,' of ', lxys)
                points = utils.math._interpolate(points,self.interpolationPoints, smoothing=self.smooth)
                        
            for x,y in points:
                #draw each point as a circle:
                utils.drawing.drawCircle(self.ctx,x,y,p_r)

        #draw the granulated points:
        for i,xy in enumerate(self.calculatedGrains):
            color = [x for x in np.random.random(3)]
            self.ctx.set_source_rgba(*color,ALPHA)

            points = xy
            if interpolateGrains:
                print('interpolating grains',i,' of ',lgrains)
                for x,y in points:
                    utils.drawing.drawCircle(self.ctx,x,y,p_r)
            

    #------------------------------
    def step(self,granulate,interpolateGrains):
        #track the amount of steps processed:
        self.itt += 1

        #process each 'set' of xy points, with its associated column of noise
        for sampleSize,xyList,noise in zip(self.snums,self.xys,self.noise):
            r = (1.0-2.0 * random((sampleSize,1))) * rMod
            scale = np.reshape(np.arange(sampleSize).astype('float'),(sampleSize,1))
            #increment the noise
            noise[:] += r * scale * self.noise_stp

            #create rotation points
            a = random(sampleSize)*TWOPI
            rnd = np.column_stack((np.cos(a),np.sin(a)))

            #get the points and modify by the noise amount
            points = xyList[:,:]
            points += rnd * self.recRes * noise

            #if necessary granulate
            if granulate:
                currentGrains = utils.math.granulate(points,grains=grains,mult=grainMult)
                #points = utils.math._interpolate(points,sampleSize,smoothing=self.smooth)
                if interpolateGrains:
                    currentGrains = utils.math._interpolate(currentGrains,self.interpolationPoints, smoothing=self.smooth)
                self.calculatedGrains.append(currentGrains)
                
            #copy the points back into the main data store
            xyList[:,:] = points
            #add in the varied points after interpolating
            self.i_xys.append(utils.math._interpolate(xyList,self.interpolationPoints,smoothing=self.smooth))

            
        
    
