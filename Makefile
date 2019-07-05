
clean:
	find . -name "*.so" -exec rm {} \;
	find . -name "*.html" -exec rm {} \;
	find . -name "*.~" -exec rm {} \;

doc:
	pandoc -f org -o gfm -o README.md chez-euler.org


