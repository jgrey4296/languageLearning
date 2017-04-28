import bmesh
import bpy



#Create an empty mesh to manipulate
#The mesh will free when transferred to an object
def createMeshData(name):
    return bmesh.new()

#after manipulating a mesh to completion, make an object out of it:
def meshDataToObject(meshData,meshName,objName,free=True):
    mesh = bpy.data.meshes.new(meshName)
    meshData.to_mesh(mesh)
    if free:
        meshData.free()
    obj = bpy.data.object.new(objName,mesh)
    bpy.context.scene.objects.link(obj)
    return obj

#straight forward:
def applyToSelectedObjects(function):
    for obj in bpy.context.selected_objects:
        function(obj)


def shiftMode(modeName):
    bpy.ops.object.mode_set(mode=modeName)
        


#IO:
def loadFile(file):
    bpy.ops.wm.open_mainfile(filepath=file)

def saveFile(file):
    bpy.ops.wm.save_as_mainfile(filepath=file)
    
    
