
#---- SOURCES
MAIN = main.cpp
HEADER = CApp
SOURCE = CApp

#---- TEST MACROS
TESTSRC = SDL_test_suite
TESTLIB = -I/Users/jgrey/github/_useful_libs/unittest++/include -L/Users/jgrey/github/_useful_libs/unittest++ -lUnitTest++
TEST_BIN = TESTS_SDL
TESTDIR = tests/

#---- COMPILER
GPP = g++ -std=gnu++14
FLAGS = -g -O0 -Wall -Wextra -Wpedantic -Wno-strict-aliasing -Wstrict-overflow -Wno-missing-field-initializers

#---- DIRECTORIES
SRCDIR = src/
BUILDDIR = build/

#---- LIBRARIES
OPENGL = -framework OpenGL -framework GLUT -framework GLKit
SDL = -F/Users/jgrey/Library/Frameworks -framework SDL2
BOOST = -isystem/usr/local/Cellar/boost/1.60.0_1
LUA = -isystem/opt/local/include -L/opt/local/lib -llua
LIBS = ${OPENGL} ${SDL} ${BOOST} 
POSTLIBS = ${LUA}

#---- LEAK CHECKING
VG_OPTIONS = --leak-check=full
VALGRIND = valgrind 

#---- OUTPUT EXEINCUTABLE
EXECUTABLE = SDL_TEST

all : build run

run :
	@echo "\n\n\n----\nRUNNING\n----\n"
	@./$(BUILDDIR)${EXECUTABLE}

memcheck : build
	${VALGRIND} ${VG_OPTIONS} ${BUILDDIR}${EXECUTABLE}

build : lint
	${GPP} ${FLAGS} $(addprefix ${SRCDIR},${SOURCE:=.cpp}) ${addprefix ${SRCDIR},${MAIN}} ${LIBS} -o ${BUILDDIR}${EXECUTABLE} ${POSTLIBS}

test :
	g++ ${FLAGS} ${addprefix ${SRCDIR},${SOURCE:=.cpp}} $(addprefix $(TESTDIR),$(TESTSRC:=.cpp)) -o $(BUILDDIR)${TEST_BIN} ${TESTLIB} ${LIBS}
	@echo "\n\n\n----Testing----\n"
	@${BUILDDIR}${TEST_BIN}

lint :
	@echo "TODO: linting"
	#cppcheck ${SRCDIR}

clean :
	-rm -r ${BUILDDIR}
	mkdir ${BUILDDIR}

.PHONY : all build clean
