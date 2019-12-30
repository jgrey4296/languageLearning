import bpy
import networkx

print(networkx)

bpy.data.objects["Cube"].data.vertices[0].co.x += 2.0

bpy.ops.wm.save_as_mainfile(filepath="./simpleTest.blend")
