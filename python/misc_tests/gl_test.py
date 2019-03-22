import OpenGL
from OpenGL.GL import *

import numpy, math, sys, os
import glfw

strVS = """
#version 330 core

layout(location = 0) in vec3 aVert;

uniform mat4 uMVMatrix;
uniform mat4 uPMatrix;
uniform float uTheta;

out vec2 vTexCoord;

void main(){
	//rotational transform
	mat4 rot = mat4(
			vec4(cos(uTheta), sin(uTheta), 0.0,0.0),
			vec4(-sin(uTheta), cos(uTheta), 0.0,0.0),
			vec4(0.0, 0.0, 1.0, 0.0),
			vec4(0.0,0.0,0.0,1.0)
			);

	//transform vertex
	gl_Position = uPMatrix * uMVMatrix * rot * vec4(aVert,1.0);
	//set texture coordinate
	vTexCoord = aVert.xy + vec2(0.5,0.5);
}
"""

strFS = """
#version 330 core

in vec2 vTexCoord;
uniform sampler2D tex2D;
uniform bool showCircle;

out vec4 fragColor;

void main(){
	if(showCircle){
		//discard fragment outside circle
		if(distance(vTexCoord, vec2(0.5,0.5)) > 0.5){
			discard;
		}else{
			fragColor = texture(tex2D,vTexCoord);
		}
	}else{
		fragColor = texture(tex2D,vTexCoord);
	}
}
"""

class Scene:
    def __init__(self):
