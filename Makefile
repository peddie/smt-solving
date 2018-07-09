all: smt-solving.html

PNGS = Stabilized_omega.png Stabilized_theta.png Unstabilized_omega.png Unstabilized_theta.png

smt-solving.html: smt-solving.md pendulum-diagram.svg $(PNGS)
	pandoc --webtex https://latex.codecogs.com/svg.latex? -s -t slidy -o $@ $<

pendulum-diagram.svg: pendulum-diagram.pdf
	pdftocairo -svg $< $@

pendulum-diagram.pdf: pendulum-diagram.tex
	pdflatex $<

$(PNGS): Pendulum.hs
	stack build || cabal build
	stack exec pendulum || cabal run pendulum