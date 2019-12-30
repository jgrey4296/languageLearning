# $Id: Makefile,v 1.2 2016-01-19 17:49:34-08 - - $
OS = $(shell uname)
GPP  = g++ -std=gnu++14
FLAGS = -g -O0 -Wall -Wextra

#LIBS = -lGLU -lX11 -lrt -lm
ifeq ($(OS),Linux)
	FLAGS += -lGL -lglut -lGLU
endif
ifeq ($(OS),Darwin)
	FLAGS += -framework glut -framework OpenGL
endif
ifeq ($(OS),Cygwin)
	FLAGS += -lopengl32 -lglut32
endif


all : hello-gl

hello-gl : hello-gl.cpp
	${GPP} ${FLAGS} hello-gl.cpp -o hello-gl 

ci : Makefile hello-gl.cpp
	cpplint.py.perl hello-gl.cpp
	checksource Makefile hello-gl.cpp
	cid Makefile hello-gl.cpp

test : all
	hello-gl &

clean :
	rm hello-gl
