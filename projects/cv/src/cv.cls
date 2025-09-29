% Intro Options
\ProvidesClass{deedy-resume-reversed}[2018/09/22 CV class]
\NeedsTeXFormat{LaTeX2e}
\DeclareOption{print}{\def\@cv@print{}}
\DeclareOption*{%
  \PassOptionsToClass{\CurrentOption}{article}
}
\ProcessOptions\relax
\LoadClass{article}

% Package Imports
\usepackage[hmargin=0.75cm, vmargin=0.25cm]{geometry}
\usepackage[hidelinks]{hyperref}
\usepackage{ulem}
\usepackage{soul}
\newcommand{\uhref}[2]{\href{#1}{\dotuline{#2}}}

% Publications
\usepackage{cite}
\renewcommand\refname{\vskip -1.5cm}

% Color definitions
\usepackage[usenames,dvipsnames]{xcolor}
\definecolor{date}{HTML}{666666}
\definecolor{primary}{HTML}{1b1b1b}
\definecolor{headings}{HTML}{1b1b1b}
\definecolor{subheadings}{HTML}{1b1b1b}

\usepackage{lmodern}

% Name command
\newcommand{\namesection}[2]{
  \centering{
    \fontsize{40pt}{60pt}
    \bfseries #1
  } \\[5pt]
  \centering{
    \color{headings}
  \sffamily\fontsize{11pt}{14pt}\selectfont #2}
  \noindent\makebox[\linewidth]{\color{headings}\rule{\paperwidth}{0.4pt}}
  \vspace{-15pt}
}

% Section separators
\usepackage{titlesec}
\titlespacing{\section}{0pt}{0pt}{0pt}
\titlespacing{\subsection}{0pt}{0pt}{0pt}
\newcommand{\sectionsep}{\vspace{8pt}}

% Headings command
\titleformat{\section}{\color{headings}
  \scshape\rmfamily\fontsize{16pt}{24pt}\selectfont
\raggedright\uppercase}{}{0em}{}

% Subheadings command
\titleformat{\subsection}{
\color{subheadings}\rmfamily\fontsize{12pt}{12pt}\selectfont\bfseries\uppercase}{}{0em}{}

\newcommand{\runsubsection}[1]{
  \color{subheadings}\rmfamily\fontsize{12pt}{12pt}\selectfont\bfseries\uppercase
{#1} \normalfont}

% Descriptors command
\newcommand{\descript}[1]{
  \color{subheadings}\raggedright\scshape\sffamily\fontsize{11pt}{13pt}\selectfont
{#1 \\} \normalfont}

% Location command
\newcommand{\location}[1]{
  \color{subheadings}\raggedright\sffamily\fontsize{10pt}{12pt}\selectfont
{#1\\} \normalfont}

% Bullet Lists with fewer gaps command
\newenvironment{tightemize}{
  \vspace{-\topsep}
\begin{itemize}\itemsep1pt \parskip0pt \parsep0pt}
    {
\end{itemize}\vspace{-\topsep}}
