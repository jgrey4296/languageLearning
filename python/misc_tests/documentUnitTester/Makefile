INSTALL_DIR= $(shell echo $(JG_PYLIBS))
LIBNAME=doctester

install : clean uninstall
	cp -r ./${LIBNAME} ${INSTALL_DIR}/${LIBNAME}

uninstall :
	rm -r ${INSTALL_DIR}/${LIBNAME}

clean :
	find . -name "__pycache__" | xargs rm -r
	find . -name "*.pyc" | xargs rm

