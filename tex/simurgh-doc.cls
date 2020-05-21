\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{simurgh-doc}
         [2013/09/18 v0.01a simurgh documentation class]
\DeclareOption*{%
    \PassOptionsToClass  {\CurrentOption}{ltxdoc}}
\ProcessOptions
\LoadClass{ltxdoc}
\RequirePackage{xcolor}
\RequirePackage{minted}
\RequirePackage{graphicx}
\RequirePackage{fontspec}
\RequirePackage[pdfborder={0 0 0},bookmarksnumbered]{hyperref}
\RequirePackage[left=4cm,right=2.25cm,top=2.5cm,bottom=2.5cm,nohead]{geometry}
\RequirePackage{makeidx}
\RequirePackage{microtype}

\def\simurgh{\textsc{simurgh}}

\newbox\simurghlogobox
\sbox\simurghlogobox{\includegraphics[height=0.5in,keepaspectratio=true]{simurgh-logo}}

\def\ps@docheadings{%
      \let\@oddhead\@empty
      \def\@oddfoot{\parbox{\textwidth}{\hfill\raisebox{0.2in}{\leftmark}\quad\raisebox{0.2in}{\thepage}\qquad\usebox\simurghlogobox}}%
      \let\@mkboth\markboth
    \def\sectionmark##1{%
      \markboth {%
        ##1}{}}}
        
\renewcommand\section{\@startsection {section}{1}{\z@}%
                                   {-3.5ex \@plus -1ex \@minus -.2ex}%
                                   {2.3ex \@plus.2ex}%
                                   {\normalfont\Large\bfseries\color{red!65!black}}}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\normalfont\large\bfseries\color{red!65!black}}}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\normalfont\normalsize\bfseries\color{red!65!black}}}
\renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
                                    {3.25ex \@plus1ex \@minus.2ex}%
                                    {-1em}%
                                    {\normalfont\normalsize\bfseries\color{red!65!black}}}
\renewcommand\subparagraph{\@startsection{subparagraph}{5}{\parindent}%
                                       {3.25ex \@plus1ex \@minus .2ex}%
                                       {-1em}%
                                      {\normalfont\normalsize\bfseries\color{red!65!black}}}        
                                      
                                      
\def\@undottedtocline#1#2#3#4#5{%
  \ifnum #1>\c@tocdepth \else
    \vskip \z@ \@plus.2\p@
    {\leftskip #2\relax \rightskip \@tocrmarg \parfillskip -\rightskip
     \parindent #2\relax\@afterindenttrue
     \interlinepenalty\@M
     \leavevmode
     \@tempdima #3\relax
     \advance\leftskip \@tempdima \null\nobreak\hskip -\leftskip
     {#4}\nobreak
        \hfill
     \nobreak
     \hb@xt@\@pnumwidth{\hfil\normalfont \normalcolor #5}%
     \par}%
  \fi}
\renewcommand*\l@subsection{\@undottedtocline{2}{\z@}{2.3em}}
\renewcommand*\l@subsubsection{\@undottedtocline{3}{\z@}{3.2em}}
\renewcommand*\l@paragraph{\@undottedtocline{4}{\z@}{4.1em}}
\renewcommand*\l@subparagraph{\@undottedtocline{5}{\z@}{5em}}
\renewcommand*\l@figure{\@undottedtocline{1}{\z@}{2.3em}}
\let\l@table\l@figure

\renewcommand\tableofcontents{%
    \section*{\contentsname
        \@mkboth{%
           \contentsname}{\contentsname}}%
    \@starttoc{toc}%
    }

\providecommand\href[2]{\texttt{#1}}

\colorlet{examplefill}{yellow!80!black}
\definecolor{graphicbackground}{rgb}{0.96,0.96,0.8}
\definecolor{codebackground}{rgb}{0.8,0.8,1}

\newcommand{\smplfile}[1]{\par\smallskip\noindent\llap{\color{blue!65!black}\vtop to0pt{\llap{\textbf{\textsc{sample}}}\vskip-3pt\llap{\texttt{#1}}\vss}\ \ }}
\newcommand{\defaultkeyval}{\llap{\color{orange!85!black}\textbf{\textsc{default}}\hskip\leftmargin}}

\newenvironment{simurghmanualentry}{\list{}{\leftmargin=2em\itemindent-\leftmargin\def\makelabel##1{\hss##1}}}{\endlist}
\newcommand\simurghmanualentryheadline[1]{\itemsep=0pt\parskip=0pt\item\strut#1\par\topsep=0pt}
\newcommand\simurghmanualbody{\parskip3pt}

\newenvironment{command}[1]{
  \begin{simurghmanualentry}
    \extractcommand#1\@@
    \simurghmanualbody
}
{
  \end{simurghmanualentry}
}

\def\extractcommand#1#2\@@{%
  \simurghmanualentryheadline{\declare{\texttt{\string#1}}#2}%
  \removeats{#1}%
  \index{\strippedat @\protect\myprintocmmand{\strippedat}}}


\renewenvironment{environment}[1]{
  \begin{simurghmanualentry}
    \extractenvironement#1\@@
    \simurghmanualbody
}
{
  \end{simurghmanualentry}
}

\def\extractenvironement#1#2\@@{%
  \simurghmanualentryheadline{{\ttfamily\char`\\begin\char`\{\declare{#1}\char`\}}#2}%
  \simurghmanualentryheadline{{\ttfamily\ \ }\meta{environment contents}}%
  \simurghmanualentryheadline{{\ttfamily\char`\\end\char`\{\declare{#1}\char`\}}}%
  \index{#1@\protect\texttt{#1} environment}%
  \index{Environments!#1@\protect\texttt{#1}}}
  
\newenvironment{package}[1]{
  \begin{simurghmanualentry}
    \simurghmanualentryheadline{{\ttfamily\char`\\usepackage\opt{[\meta{options}]}\char`\{\declare{#1}\char`\}}}
    \index{#1@\protect\texttt{#1} package}%
    \index{Packages and files!#1@\protect\texttt{#1}}%
    \simurghmanualbody
}
{
  \end{simurghmanualentry}
}



\newenvironment{filedescription}[1]{
  \begin{simurghmanualentry}
    \simurghmanualentryheadline{File {\ttfamily\declare{#1}}}%
    \index{#1@\protect\texttt{#1} file}%
    \index{Packages and files!#1@\protect\texttt{#1}}%
    \simurghmanualbody
}
{
  \end{simurghmanualentry}
}


\newenvironment{packageoption}[1]{
  \begin{simurghmanualentry}
    \simurghmanualentryheadline{{\ttfamily\char`\\usepackage[\declare{#1}]\char`\{simurgh\char`\}}}
    \index{#1@\protect\texttt{#1} package option}%
    \index{Package options for \textsc{simurgh}!#1@\protect\texttt{#1}}%
    \simurghmanualbody
}
{
  \end{simurghmanualentry}
}



\newcommand\opt[1]{{\color{black!50!green}#1}}
\newcommand\ooarg[1]{{\ttfamily[}\meta{#1}{\ttfamily]}}

\let\oarg\simurgh@undefined
\providecommand\oarg[1]{%
  {\color{black!50!green}{\ttfamily[}\meta{#1}{\ttfamily]}}}
  



\def\opt{\afterassignment\simurghmanualopt\let\next=}
\def\simurghmanualopt{\ifx\next\bgroup\bgroup\color{black!50!green}\else{\color{black!50!green}\next}\fi}




\def\pdf{\textsc{pdf}}


{
  \makeatletter
  \global\let\myempty=\@empty
  \global\let\mygobble=\@gobble
  \makeatother
  \gdef\getridofats#1@#2\relax{%
    \def\getridtest{#2}%
    \ifx\getridtest\myempty%
      \expandafter\def\expandafter\strippedat\expandafter{\strippedat#1}
    \else%
      \expandafter\def\expandafter\strippedat\expandafter{\strippedat#1\protect\printanat}
      \getridofats#2\relax%
    \fi%
  }

  \gdef\removeats#1{%
    \let\strippedat\myempty%
    \edef\strippedtext{\stripcommand#1}%
    \expandafter\getridofats\strippedtext @\relax%
  }
  
  \gdef\stripcommand#1{\expandafter\mygobble\string#1}
}

\def\printanat{\char`\@}

\def\declare{\afterassignment\simurghmanualdeclare\let\next=}
\def\simurghmanualdeclare{\ifx\next\bgroup\bgroup\color{red!75!black}\else{\color{red!75!black}\next}\fi}

\def\example{\par\smallskip\noindent\textit{Example: }}

\def\itemoption#1{\item \declare{\texttt{#1}}%
  \indexoption{#1}%
}

\def\indexoption#1{%
  \index{#1@\protect\texttt{#1} option}%
  \index{Options!#1@\protect\texttt{#1}}%
}

\let\textoken=\command
\let\endtextoken=\endcommand

\def\myprintocmmand#1{\texttt{\char`\\#1}}


\def\index@prologue{\section*{Index}\addcontentsline{toc}{section}{Index}}
\c@IndexColumns=2
  \def\theindex{\@restonecoltrue
    \columnseprule \z@  \columnsep 35\p@
    \twocolumn[\index@prologue]%
     \@mkboth{\indexname}%
                     {\indexname}%
       \parindent -30pt
       \columnsep 15pt
       \parskip 0pt plus 1pt
       \leftskip 30pt
       \rightskip 0pt plus 2cm
       \small
       \def\@idxitem{\par}%
    \let\item\@idxitem \ignorespaces}
  \def\endtheindex{\onecolumn}
\def\noindexing{\let\index=\@gobble}
  

\endinput
