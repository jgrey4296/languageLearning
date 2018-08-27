
PYTHON = /Users/jgrey/anaconda/lib/libpython3.5m.dylib
APP_DIR = ./dist/main.app/Contents

SRC_DATA_DIR = ./timProgram/data
DATA_DIR = ${APP_DIR}/Resources/data

all : clean
	-python setup.py py2app
	cp ${PYTHON} ${APP_DIR}/Frameworks/
	cp -r ${SRC_DATA_DIR} ${DATA_DIR}


clean:
	rm -r dist
	rm -r build
