PACKAGE = chez-biblio
VERSION = 0.1

CHEZ = scheme
INSTALL = install -D

PREFIX = ~/.chez.d
EXEC_PREFIX = ${PREFIX}
BINDIR = ${EXEC_PREFIX}/bin
LIBDIR = ${EXEC_PREFIX}/lib
INCLUDEDIR = ${PREFIX}/include
DATAROOTDIR = ${PREFIX}/share
DATADIR = ${DATAROOTDIR}
MANDIR = ${DATAROOTDIR}/man
INFODIR = ${DATAROOTDIR}/info
DOCDIR = ${DATAROOTDIR}/doc/${PACKAGE}-${VERSION}

chezversion ::= $(shell echo '(call-with-values scheme-version-number (lambda (a b c) (format \#t "~d.~d" a b)))' | ${CHEZ} -q)
schemedir = ${LIBDIR}/csv${chezversion}-site

build:
	echo "(compile-library \"chez/euler.sls\"))" | ${CHEZ} -q

install:
	find . -type f -regex ".*.so" -exec sh -c '${INSTALL} -t ${schemedir}/$$(dirname $$1) $$1' _ {} \;

clean:
	find . -name "*.so" -exec rm {} \;
	find . -name "*.html" -exec rm {} \;
	find . -name "*~" -exec rm {} \;



