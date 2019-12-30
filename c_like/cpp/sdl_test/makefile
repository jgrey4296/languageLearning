
#---- SOURCES
HEADER = CApp
SOURCE = CApp

#---- TEST FILES
TESTSRC = main
TESTLIB = -I/Users/jgrey/github/_useful_libs/unittest++/include -L/Users/jgrey/github/_useful_libs/unittest++ -lUnitTest++
TEST_BIN = TESTS_SDL

#---- COMPILER
GPP = G++ -std=gnu++14

#--- COMPILER FLAGS
FLAGS = -g -O0 -Wall -Wextra

#---- DIRECTORIES
TESTDIR = tests/
SRCDIR = src/
BUILDDIR = build/

#---- LIBRARIES
OPENGL = -framework OpenGL -framework GLUT -framework GLKit
SDL = -F/Users/jgrey/Library/Frameworks -framework SDL2
BOOST = -I/usr/local/Cellar/boost/1.60.0_1
LIBS = ${OPENGL} ${SDL} ${BOOST}

#---- OUTPUT EXEINCUTABLE
EXECUTABLE = SDL_TEST

all : build run

run :
	@echo "\n\n\n----\nRUNNING\n----\n"
	@./$(BUILDDIR)${EXECUTABLE}

build : lint
	g++ ${FLAGS} $(addprefix ${SRCDIR},${SOURCE:=.cpp}) ${LIBS} -o ${BUILDDIR}${EXECUTABLE}

test :
	@echo "\n\n\n----Testing\n----\n"
	g++ ${FLAGS} $(addprefix $(TESTDIR),$(TESTSRC:=.cpp)) -o $(BUILDDIR)test ${TESTLIB}
	@${BUILDDIR}${TEST_BIN}

lint :
	@echo "TODO: linting"

clean :
	-rm *.o
	-rm -r *.dSYM
	-rm ${EXECUTABLE}


.PHONY : all build clean
