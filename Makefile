
ilc-calc.pdf: Makefile ilc-calc.ott
	ott -merge true -i ilc-calc.ott -o ilc-calc.tex
	pdflatex ilc-calc.tex

clean:
	rm -rf \
		ilc-calc.pdf ilc-calc.tex \
