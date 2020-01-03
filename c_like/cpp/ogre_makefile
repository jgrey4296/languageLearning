#--- SOURCES
SRC_ROOT = src
SRCS = ${shell find ${SRC_ROOT} -name "*.cpp"}

#--- BUILDING
BUILD_DIR = build

#--- Compiler
GPP = g++ -std=gnu++14
FLAGS = -g -O0 -Wall -Wextra -Wpedantic -Wno-strict-aliasing -Wstrict-overflow -Wno-missing-field-initializers

#--- Libs
#libs are frameworks that I can't change have their warnings suppressed by
#using -isystem and -iframework:
OPENGL = -framework OpenGL -framework GLUT -framework GLKit
#SDL = -F/Users/jgrey/Library/Frameworks -framework SDL2
BOOST = -isystem/usr/local/Cellar/boost/1.60.0_1 -L/usr/local/Cellar/boost/1.60.0_1/lib -lboost_system
OGRE = -iframework./Frameworks -framework Ogre

#-isystem/Users/jgrey/OgreSDK/include
LIBS = ${OPENGL} ${SDL} ${BOOST} ${OGRE}


#--- EXE
EXECUTABLE = ogreTest

all : build run

run :
	echo "\n\nRunning\n\n"
	${BUILD_DIR}/${EXECUTABLE}

build : ${SRCS}
	${GPP} ${FLAGS} ${LIBS} ${SRCS} -o ${BUILD_DIR}/${EXECUTABLE}

clean :
	-rm -r ${BUILD_DIR}
	mkdir ${BUILD_DIR}

.PHONY : all build clean
