PACKAGE = chez-euler
CHEZ = scheme
INSTALL = install -D
prefix = ~/.chez.d
schemedir = ${prefix}

build:
	echo "(compile-library \"chez/euler.sls\"))" | ${CHEZ} -q

install:
	find . -type f -regex ".*.so" -exec sh -c '${INSTALL} -t ${schemedir}/$$(dirname $$1) $$1' _ {} \;

clean:
	find . -name "*.so" -exec rm {} \;
	find . -name "*.html" -exec rm {} \;
	find . -name "*~" -exec rm {} \;



