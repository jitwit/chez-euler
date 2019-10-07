package = chez-euler
version = "0.1"
chez = scheme
install = install -D
prefix = ~/.chez.d
schemedir = ${prefix}

build:
	echo "(compile-library \"euler.sls\"))" | ${chez} -q

install:
	find . -type f -regex ".*.so" -exec sh -c '${install} -t ${schemedir}/$$(dirname $$1) $$1' _ {} \;

clean:
	find . -name "*.so" -exec rm {} \;
	find . -name "*.html" -exec rm {} \;
	find . -name "*~" -exec rm {} \;



