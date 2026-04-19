%
% InsightArticle.cls for the Insight Journal
%

\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{InsightArticle}
             [1998/02/25 Document class (Insight Article)]

%
%  Change this to say a4paper instead of letterpaper if you want A4.  These
%  are the latex defaults.
%
\newcommand{\itk@paper}{letterpaper}
\newcommand{\itk@ptsize}{11pt}


%
%  Commands for image and figure captions
%
\newcommand{\itkcaption}[2][]{\caption[{#1}]{\small \textsf{{#2}} \normalsize}}
\newcommand{\itkpiccaption}[2][]{\piccaption[{#1}]{\small \textsf{{#2}} \normalsize}}


%  These set up the fonts for the documents.
%
%  The "times" package makes the default font the PostScript Times
%  font, which makes for smaller PostScript and a font that more people
%  like.
%
\RequirePackage{times}\typeout{Using Times instead of Computer Modern.}


% Change the options here to get a different set of basic options,  This
% is where to add things like "a4paper" or "10pt".
%
\LoadClass[\itk@paper,\itk@ptsize]{article}



\setcounter{secnumdepth}{1}



% Optional packages:
%
% If processing of these documents fails at your TeX installation,
% these may be commented out (independently) to make things work.
% These are both supplied with the current version of the teTeX
% distribution.
%
% The "fancyhdr" package makes nicer page footers reasonable to
% implement, and is used to put the chapter and section information in
% the footers.
%
\RequirePackage{fancyhdr}\typeout{Using fancier footers than usual.}


% Adding the package ifpdf in order to deal with the cases where
% pdflatex is used instead of latex.
\RequirePackage{ifpdf}\typeout{Using ifpdf for the template}


% Required package:
%
% This gives us all the Insight-specific markup that we really want.
% This should come last.  Do not change this.
%
\RequirePackage{InsightJournal}

% support for module synopsis sections:
\newcommand{\itk@ModSynopsisFilename}{\jobname.syn}


% need to do one of these....
\newcommand{\itk@doHorizontalRule}{\rule{\textwidth}{1pt}}

% Define command to make reference to on-line Doxygen documentation
\newcommand{\doxygen}[1]{
\href{https://www.itk.org/Doxygen/html/classitk_1_1#1.html}{\code{itk::#1}}}

% Define command to make reference to on-line Doxygen documentation
\newcommand{\subdoxygen}[2]{
\href{https://www.itk.org/Doxygen/html/classitk_1_1#1_1_1#2.html}{\code{itk::#1::#2}}}

% Define command for the standard comment introducing classes with similar functionalities
\newcommand{\relatedClasses}{
\textbf{The following classes provide similar functionality:}}


% Change the title page to look a bit better, and fit in with the
% fncychap ``Bjarne'' style a bit better.
%
\renewcommand{\maketitle}{
  \itk@doHorizontalRule
  \ifpdf
    \@ifundefined{pdfinfo}{}{{
      % This \def is required to deal with multi-line authors; it
      % changes \\ to ', ' (comma-space), making it pass muster for
      % generating document info in the PDF file.
      \def\\{, }
        \pdfinfo{
          /Author (\@author)
          /Title (\@title)
        }
    }}
  \else
     % else nothing
  \fi
  \begin{flushright}
    {\rm\Huge\itk@HeaderFamily \@title} \par
    {\em\large\itk@HeaderFamily \itk@release} \par
    \vspace{25pt}
    {\Large\itk@HeaderFamily \@author} \par
    \vspace{25pt}
    \@date \par
    \itk@authoraddress \par
  \end{flushright}
  \@thanks
  \setcounter{footnote}{0}
  \let\thanks\relax\let\maketitle\relax
  \gdef\@thanks{}\gdef\@author{}\gdef\@title{}
}


\let\itk@OldTableofcontents=\tableofcontents
\renewcommand{\tableofcontents}{
  \begingroup
    \parskip = 0mm
    \itk@OldTableofcontents
  \endgroup
  \itk@doHorizontalRule
  \vspace{12pt}
  \itk@doing@page@targetstrue
}

% Fix the theindex environment to add an entry to the Table of
% Contents; this is much nicer than just having to jump to the end of
% the book and flip around, especially with multiple indexes.
%
\let\itk@OldTheindex=\theindex
\renewcommand{\theindex}{
  \clearpage
  \itk@OldTheindex
  \addcontentsline{toc}{section}{\indexname}
}

\@ifundefined{fancyhf}{
  \pagestyle{plain}}{
  \pagestyle{normal}}		% start this way; change for
\pagenumbering{arabic}		% ToC & chapters
\setcounter{secnumdepth}{2}

\thispagestyle{empty}
