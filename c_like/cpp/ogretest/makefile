#--- SOURCES
SRC_DIR = src/
SRCS = main
#--- BUILDING
BUILD_DIR = build

#--- Compiler
GPP = g++ -std=gnu++14
FLAGS = -g -O0 -Wall -Wextra -Wpedantic -Wno-strict-aliasing -Wstrict-overflow -Wno-missing-field-initializers

#--- Libs
#libs are frameworks that I can't change have their warnings suppressed by
#using -isystem and -iframework:
OPENGL = -framework OpenGL -framework GLUT -framework GLKit
SDL = -F/Users/jgrey/Library/Frameworks -framework SDL2
BOOST = -isystem/usr/local/Cellar/boost/1.60.0_1 -L/usr/local/Cellar/boost/1.60.0_1/lib -lboost_system
OGREFrameworks = RenderSystem_GL Plugin_OctreeSceneManager
OGRE = -iframework/Users/jgrey/OgreSDK/lib/macosx/Release -framework Ogre

#-isystem/Users/jgrey/OgreSDK/include
LIBS = ${OPENGL} ${SDL} ${BOOST} ${OGRE}


#--- EXE
EXECUTABLE = ogreTest

all : build run

run :
	echo "\n\nRunning\n\n"
	${BUILD_DIR}/${EXECUTABLE}

build :
	${GPP} ${FLAGS} ${LIBS} ${addprefix ${SRC_DIR},${SRCS:=.cpp}} -o ${BUILD_DIR}/${EXECUTABLE}


clean :
	-rm -r ${BUILD_DIR}
	mkdir ${BUILD_DIR}


.PHONY : all build clean




#--- dylib search path changing
#update the ogre framework path
#CAN BE CHECKED WITH: otool -L ${EXECUTABLE}
# OGRE_FRAMEWORK_PATH = /Users/jgrey/OgreSDK/lib/macosx/Release
# INSTALL_NAME_TOOL = install_name_tool

# OGRE_PATH_PAIR = @executable_path/../Frameworks/Ogre.framework/Versions/1.9.0/Ogre ${OGRE_FRAMEWORK_PATH}/Ogre.framework/Ogre

# GL_RENDER_PATH_PAIR = @executable_path/../Frameworks/RenderSystem_GL.framework/Versions/1.9.0/RenderSystem_GL ${OGRE_FRAMEWORK_PATH}/RenderSystem_GL.framework/RenderSystem_GL

# OCTREE_PATH_PAIR = @executable_path/../Frameworks/Plugin_OctreeSceneManager.framework/Versions/1.9.0/Plugin_OctreeSceneManager ${OGRE_FRAMEWORK_PATH}/Plugin_OctreeSceneManager.framework/Plugin_OctreeSceneManager
# #${INSTALL_NAME_TOOL} -change ${OGRE_PATH_PAIR} -change ${GL_RENDER_PATH_PAIR} -change ${OCTREE_PATH_PAIR} ${BUILD_DIR}/${EXECUTABLE}
