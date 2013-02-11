
# you should use the title of the article instead
ARTICLE_TITLE = Parallel algorithms for erosions and dilations of label images# spaces are not allowed
empty:=
space:= $(empty) $(empty)
PDF_NAME:= $(subst $(space),_,$(ARTICLE_TITLE))

NAME = Article

BIBS=$(wildcard *.bib)

PNGS=$(notdir $(wildcard Pics/*.png))
JPGS=$(notdir $(wildcard Pics/*.jpg))
DIAS=$(notdir $(wildcard Pics/*.dia))
FIGS=$(notdir $(wildcard Pics/*.fig))
EPSS=$(notdir $(wildcard Pics/*.eps))
TIFS=$(notdir $(wildcard Pics/*.tif))
PLOTS=$(notdir $(wildcard Pics/*.gnuplot))
PDFS=$(notdir $(wildcard Pics/*.pdf))

OUTPUT_EPS=$(PNGS:png=eps) $(JPGS:jpg=eps) $(DIAS:dia=eps) $(FIGS:fig=eps) $(TIFS:tif=eps) $(PLOTS:gnuplot=eps) $(PDFS:pdf=eps) $(EPSS)

all: pdf


ps: ${NAME}.ps
pdf: ${PDF_NAME}.pdf
dvi: ${NAME}.dvi
bbl: ${NAME}.bbl
eps: $(OUTPUT_EPS)


%.eps: Pics/%.png
	convert $< $@

%.eps: Pics/%.jpg
	convert $< $@

%.eps: Pics/%.dia
	dia --nosplash -e $@ $<

%.eps: Pics/%.fig
	fig2dev -L eps $< $@

%.eps: Pics/%.gnuplot
	(cd Pics; gnuplot $<) > $@

%.eps: Pics/%.pdf
	pdf2ps $< $@

${NAME}.ps: ${NAME}.dvi
	dvips -R0 -o ${NAME}.ps ${NAME}.dvi

%.eps: Pics/%.tif
	convert $< $@


${PDF_NAME}.pdf: ${NAME}.ps
	ps2pdf -dMaxSubsetPct=100 -dPDFSETTINGS=/prepress -dCompatibilityLevel=1.4 \
         -dSubsetFonts=true -dEmbedAllFonts=true \
         -dAutoFilterColorImages=false \
         -dColorImageFilter=/FlateEncode -dGrayImageFilter=/FlateEncode \
	 -dMonoImageFilter=/FlateEncode \
	 ${NAME}.ps ${PDF_NAME}.pdf


${NAME}.bbl: ${BIBS} ${NAME}.aux
	bibtex ${NAME}


${NAME}.dvi: ${NAME}.tex  ${BIBS}
	$(MAKE) eps
	latex -interaction=nonstopmode ${NAME}.tex
	bibtex ${NAME}
	latex -interaction=nonstopmode ${NAME}.tex
	latex -interaction=nonstopmode ${NAME}.tex


${NAME}.aux: ${NAME}.tex  ${BIBS}
	latex ${NAME}.tex


clean:
	rm -f ${NAME}.dvi ${NAME}.ps ${PDF_NAME}.pdf ${NAME}.aux ${NAME}.log ${NAME}.bbl ${NAME}.blg ${NAME}.toc ${NAME}.out  ${NAME}.brf $(DIAS:dia=eps) $(PNGS:png=eps) $(FIGS:fig=eps) $(PLOTS:gnuplot=eps)

