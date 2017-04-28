# Drawing heavily from https://github.com/a1studmuffin/SpaceshipGenerator/
import bpy
import bmesh
from mathutils import Vector, Matrix
import numpy as np
from numpy.random import choice
from random import randrange
#import imp
#imp.reload(script) for hot reloading

#Utilities for doing stuff with bmeshs in blender.
class JGMeshContext:

    @staticmethod
    def Test():
        with JGMeshContext() as t:
            t.add_cube()
            new_face = t.extrude_face(t.mesh.faces[:][0], 2)[0]
            new_face_2 = t.extrude_face(new_face, 3)[0]
            for face in t.faces_adjacent_to_face(new_face_2):
                    t.extrude_face(face,3)
            chosen = choice(t.faces(),4)
            t.delete_faces(chosen[:])

                    
    
    def __init__(self, data=None, obj=None):
        if data is None:
            data = { 'type': 'cube', 'size': 1, 'matrix': Matrix(), 'name': 'default' }
        self.data = data
        self.obj = obj
        self.mesh = None
    
    def __enter__(self):
        """ Create a mesh, give it to the context for use, wrapped in this """
        bm = bmesh.new()
        if self.obj is not None:
            bm.from_mesh(self.obj)
        self.mesh = bm
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        """ Given editing has finished, convert to an object """
        print(exc_type, exc_value)
        if self.obj is not None:
            self.mesh.to_mesh(self.obj.data)
            self.mesh.free()
            bpy.context.scene.update()
            return
        
        me = bpy.data.meshes.new('Mesh')
        self.mesh.to_mesh(me)
        self.mesh.free()
        scene = bpy.context.scene
        obj = bpy.data.objects.new(self.data['name'], me)
        scene.objects.link(obj)

    
    def add_grid(self, x, y, size, matrix=None):
        if matrix is None: matrix = Matrix()
        
        return bmesh.ops.create_grid(self.mesh,
                                     x_segments=x,
                                     y_segments=y,
                                     size=size,
                                     matrix=matrix)

    def add_uvsphere(self, u, v, d, matrix):
        if matrix is None: matrix = Matrix()

        return bmesh.ops.create_uvsphere(self.mesh,
                                         u_segments=u,
                                         v_segments=v,
                                         diameter=d,
                                         matrix=matrix)

    def add_cone(self, ends, tris, seg, d1, d2, depth, matrix):
        if matrix is None: matrix = Matrix()

        return bmesh.ops.create_cone(self.mesh,
                                     cap_ends=ends,
                                     cap_tris=tris,
                                     segmets=seg,
                                     diameter1=d1,
                                     diameter2=d2,
                                     depth=depth,
                                     matrix=matrix)
    
    def add_circle(self, ends, tris, seg, d, matrix):
        return bmesh.ops.create_circle(self.mesh,
                                       cap_endgs=ends,
                                       cap_tris=tris,
                                       segments=seg,
                                       diameter=d,
                                       matrix=matrix)

    def add_cube(self, size=None, matrix=None):
        if size is None and 'size' in self.data:
            size = self.data['size']
        if matrix is None and 'matrix' in self.data:
            matrix = self.data['matrix']
        return bmesh.ops.create_cube(self.mesh, size=size, matrix=matrix)
                    
    def faces(self):
        return self.mesh.faces[:]
        
    def extrude_face(self, face, amnt):
        new_faces = bmesh.ops.extrude_discrete_faces(self.mesh,
                                                     faces=[face])['faces'][:]
        new_face = new_faces[0]
        bmesh.ops.translate(self.mesh,
                            vec= new_face.normal * amnt,
                            verts=new_face.verts)
        return new_faces

    #todo: bmesh.ops.extrude_edge_only
    #todo: bmesh.ops.extrude_vert_indiv
    #todo: bmesh.ops.dissolve_verts/edges/faces
    #bmesh.ops.spin
    
    def translate(self, vec, data, matrix=None):
        if matrix is None:
            matrix = Matrix()
        targets = listVerts(data)
        
        bmesh.ops.translate(self.mesh,
                            vec=vec,
                            space=matrix,
                            verts=targets)

    def scale(self, vec, data, matrix=None):
        if matrix is None:
            matrix = Matrix()
        targets = listVerts(data)
        bmesh.ops.scale(self.mesh, vec=vec, space=matrix, verts=targets)

    def rotate(self, cen, rot, data, matrix=None):
        if matrix is None:
            matrix = Matrix()
        targets = listVerts(data)
        bmesh.ops.rotate(self.mesh, cent=cen, matrix=rot, space=matrix, verts=targets)
        
    
    def delete_faces(self, faces):
        # Enum
	# DEL_VERTS = 1, DEL_EDGES, DEL_ONLYFACES, DEL_EDGESFACES,
	# DEL_FACES, DEL_ALL, DEL_ONLYTAGGED
        if not isinstance(faces, list) and not isinstance(faces, np.ndarray):
            faces = [faces]
        bmesh.ops.delete(self.mesh, geom=faces, context=5)

    def faces_adjacent_to_face(self, face):
        edges = face.edges[:]
        faces = [x for edge in edges for x in edge.link_faces[:] if x is not face]
        return faces
    
    

        
class JGObjectContext:

    def __init__(self, name, data):
        #unselect everything
        self.theObj = bpy.data.objects[name]
        for obj in bpy.context.scene.selected_objects:
            obj.selected = False
        bpy.context.active_object = self.theObj
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        """ Not really anything to do for exiting """

        
#Utilities
def obj_to_mesh(obj):
    new_mesh = bmesh.new()
    new_mesh.from_mesh(obj.data)
    return new_mesh

def mesh_to_obj(mesh, obj):
    mesh.to_mesh(obj.data)

def listVerts(lst):
    targets = set()
    for x in lst:
        if isinstance(x, bmesh.types.BMVert):
            targets.add(x)
        elif isinstance(x, bmesh.types.BMEdge):
            targets.update(x.verts[:])
        elif isinstance(x, bmesh.types.BMFace):
            targets.update(x.verts[:])
    return list(targets)


#from  https://github.com/a1studmuffin/SpaceshipGenerator/
# Returns a rough 4x4 transform matrix for a face (doesn't handle
# distortion/shear) with optional position override.
def get_face_matrix(face, pos=None):
    x_axis = (face.verts[1].co - face.verts[0].co).normalized()
    z_axis = -face.normal
    y_axis = z_axis.cross(x_axis)
    if not pos:
        pos = face.calc_center_bounds()

    # Construct a 4x4 matrix from axes + position:
    # http://i.stack.imgur.com/3TnQP.png
    mat = Matrix()
    mat[0][0] = x_axis.x
    mat[1][0] = x_axis.y
    mat[2][0] = x_axis.z
    mat[3][0] = 0
    mat[0][1] = y_axis.x
    mat[1][1] = y_axis.y
    mat[2][1] = y_axis.z
    mat[3][1] = 0
    mat[0][2] = z_axis.x
    mat[1][2] = z_axis.y
    mat[2][2] = z_axis.z
    mat[3][2] = 0
    mat[0][3] = pos.x
    mat[1][3] = pos.y
    mat[2][3] = pos.z
    mat[3][3] = 1
    return mat

#Materials


#Test Functions to generate examples:
def Test():
    with JGMeshContext({'name': 'blah'}) as c:
        c.add_grid(10, 10, 10)
        for x in c.mesh.verts[:]:
            norm = x.normal * randrange(1,3)
            c.translate(norm, verts=[x])
        rand_faces = choice(c.faces(), 5)
        for face in rand_faces:
            c.extrude_face(face, randrange(4,6))
        

