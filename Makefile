
test.pdf: Makefile test.ott
	ott -merge true -i test.ott -o test.tex
	pdflatex test.tex

clean:
	rm -rf \
		test.pdf test.tex \
