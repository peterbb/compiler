#!/bin/ksh

preludeandcompile() {
    cat runtime.ll
    ./main $1 cps beta gen code # 
}

if [[ $# -eq 1 ]]; then
    csc -lpthread ./main.scm
    preludeandcompile $1 > tmp.txt
    cat tmp.txt | llc -tailcallopt | clang -x assembler-with-cpp -
    MALLOC_OPTIONS=X ./a.out
#    preludeandcompile $1 | llvm-as | lli 
else
    echo "usage: " $0 " <filename>"
fi
