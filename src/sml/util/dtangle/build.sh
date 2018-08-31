#!/bin/sh
#*
# \section*{Build Script}
# Shell script to bulid the heap and documentation for it
#*
#::

#:build the SML heap:
sml-cm <<EOF
CM.make();
SMLofNJ.exportFn("dtangle",DTangle.main);
EOF
##
#:run dtangle on the source files:
sml @SMLload=./dtangle --inc intro.nw \
                       -lml dtangle.sml dtangle-lexer.sml \
                       -lsh build.sh \
                       -lml sources.cm \
                       -ltex main.tex \
                       -lsh duntangle \
> dtangle.nw
##
#:run noweb and build a ps file:
 noweb -o dtangle.nw
# run it twice to get cross refs right
 latex main.tex
 latex main.tex
 dvips main.dvi -o
##