

NAME = itkBoneMorphometry

BIBTEXDATABSENAME = InsightJournal


all: ps pdf dvi


ps: ${NAME}.ps


pdf: ${NAME}.pdf


dvi: ${NAME}.dvi


bbl: ${NAME}.bbl



${NAME}.ps: ${NAME}.dvi
	dvips -o ${NAME}.ps ${NAME}.dvi


${NAME}.pdf: ${NAME}.ps
	ps2pdf ${NAME}.ps


${NAME}.bbl: ${BIBTEXDATABSENAME}.bib ${NAME}.aux
	bibtex ${NAME}


${NAME}.dvi: ${NAME}.tex  ${BIBTEXDATABSENAME}.bib
	latex -shell-escape ${NAME}.tex
	bibtex ${NAME}
	latex -shell-escape ${NAME}.tex
	latex -shell-escape ${NAME}.tex


${NAME}.aux: ${NAME}.tex  ${BIBTEXDATABSENAME}.bib
	latex -shell-escape ${NAME}.tex


clean:
	rm -r ${NAME}.dvi ${NAME}.ps ${NAME}.pdf ${NAME}.aux ${NAME}.log ${NAME}.bbl ${NAME}.blg ${NAME}.toc ${NAME}.out  ${NAME}.brf _minted-${NAME}

