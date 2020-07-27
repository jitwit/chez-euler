package = chez-euler
version = "0.1"
chez = scheme

build:
	echo "(compile-library \"euler.sls\"))" | ${chez} -q

test: build
	echo "(for-each load '(\"euler.so\" \"test/test.scm\"))" | ${chez} -q

clean:
	find . -name "*.so" -exec rm {} \;
	find . -name "*.html" -exec rm {} \;
	find . -name "*~" -exec rm {} \;



