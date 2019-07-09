TARGET = fizzbuzz.pdf

DEPENDENCIES =

LATEX = lualatex
PANDOC = pandoc
FILTERS = filters/centered.py filters/columns.py filters/only.py filters/simg.py
MDWN2X = $(PANDOC) $(foreach f,$(FILTERS),-F $(f))
MAINTEXDOCUMENT = $(patsubst %.pdf,%.tex,$(TARGET))

all: $(TARGET)

.SUFFIXES: .mdwn .tex

.mdwn.tex:
	$(MDWN2X) -t beamer $< -o $@

slides.tex: $(FILTERS) $(DEPENDENCIES) Makefile

$(TARGET): $(MAINTEXDOCUMENT) slides.tex title.tex
	$(LATEX) $(MAINTEXDOCUMENT)
	$(LATEX) $(MAINTEXDOCUMENT)

clean:
	rm -f $(TARGET) slides.tex
	rm -f *.aux *.log *.nav *.out *.snm *.toc *.vrb
	rm -f base.pdf base.tex
	rm -Rf auto

base.tex:
	$(PANDOC) -t beamer slides.mdwn -o base.tex

base.pdf:
	$(PANDOC) -t beamer slides.mdwn -o base.pdf

base: base.tex base.pdf

publish: $(TARGET)
	cp $(TARGET) ../Slides-FizzBuzzDSL.pdf

.PHONY: all base clean publish
