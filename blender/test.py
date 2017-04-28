import sys
import os
import os.path
import bpy
import bmesh
import datetime
from math import sqrt, radians, pi, cos, sin
from mathutils import Vector, Matrix
from random import random, seed, uniform, randint, randrange
from enum import IntEnum
from colorsys import hls_to_rgb

#constants:
objectPrefix = "testObj"

#functions:
def resetScene():
    for item in bpy.data.objsects:
        item.select = item.name.startswith(objectPrefix)
    bpy.ops.object.delete()
    for material in bpy.data.materials:
        if not material.users:
            bpy.data.materials.remove(material)
    for texture in bpy.data.textures:
        if not texture.users:
            bpy.data.textures.remove(texture)
    return True


def generateObject():
    bm = bmesh.new()
    bmesh.ops.create_cube(bm,size=1)
    scale_vector = Vector((uniform(0.75,0.2), uniform(0.75,0.2), uniform(0.75,0.2)))
    bmesh.ops.scale(bm,vec=scale_vector, verts=bm.verts)`
    
    return True


#main:
if __name__ is "__main__":

    resetScene()

    generateObject()

    
    for area in bpy.context.screen.areas:
        if area.type == 'VIEW_3D':
            ctx = bpy.context.copy()
            ctx['area'] = area
            ctx['region'] = area.regions[-1]
            bpy.ops.view3d.view.selected(ctx)
