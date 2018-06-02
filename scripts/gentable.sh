#!/bin/sh

cd output

exec >modelsdoc.tex

cat <<EOF
\documentclass[8pt]{article}
\usepackage{dcolumn}
\begin{document}
EOF

cat models.tex

echo "\end{document}"

pdflatex modelsdoc.tex >gentable.log 2>&1
