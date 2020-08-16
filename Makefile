.PHONY : bench clean

package = chez-euler
version = 0.1
chez = scheme
out =

build : euler.sls
	echo "(compile-library \"$<\"))" | ${chez} -q --optimize-level 3

check : euler.so
	echo "(for-each load '(\"euler.so\" \"test/test.scm\"))" | ${chez} -q

bench : bench.ss euler.so 
	scheme --optimize-level 3 --script $<

install :
	mkdir -p $(out)
	cp -r *.so $(out)

clean:
	find . -name "*.so" -exec rm {} \;
	find . -name "*.html" -exec rm {} \;
	find . -name "*~" -exec rm {} \;
