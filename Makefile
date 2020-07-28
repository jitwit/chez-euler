package = chez-euler
version = 0.1
chez = scheme
out =

build :
	echo "(compile-library \"euler.sls\"))" | ${chez} -q --optimize-level 3

check : build
	echo "(for-each load '(\"euler.so\" \"test/test.scm\"))" | ${chez} -q

install :
	mkdir -p $(out)
	cp -r *.so $(out)

clean:
	find . -name "*.so" -exec rm {} \;
	find . -name "*.html" -exec rm {} \;
	find . -name "*~" -exec rm {} \;



