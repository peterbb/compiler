#!/bin/ksh

preludeandcompile() {
    cat runtime.ll
    ./main $1 cps beta code-print
}

if [[ $# -eq 1 ]]; then
    #cat runtime.ll
    #csi -script ./main.scm $1  cps beta code-print
    #cat runtime.ll 
    #csi -script ./main.scm $1 cps beta code-print) | llc | as
    csc -lpthread ./main.scm
    #./main $1 cps beta code-print | llc
    preludeandcompile $1 | llc | gcc -x assembler-with-cpp -
else
    echo "usage: " $0 " <filename>"
fi
