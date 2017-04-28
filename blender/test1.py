import bpy
from random import random

rand = lambda x: (random() * (x * 0.5)) - (x * 0.5)

#Create a new cube
for x in range(10):
    bpy.ops.mesh.primitive_cube_add()

for x in bpy.data.objects:
    x.location = [rand(10), rand(10), rand(10)]


#update it
bpy.context.scene.update()
