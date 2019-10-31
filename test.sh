#!/bin/bash

TESTS=`ls -1 examples/ | grep ins | cut -d \. -f 1`

llvm() {
    echo "TESTING LLVM..."
    for t in $TESTS; do
        ./insc_llvm examples/$t.ins
        lli examples/$t.bc > examples/$t.myout
    done;
    compare_outs examples
    echo "TESTING LLVM DONE"
}

jvm() {
    echo "TESTING JVM..."
    for t in $TESTS; do
        ./insc_jvm examples/$t.ins
        `java -cp examples $t > examples/$t.myout`
    done;
    compare_outs examples
    echo "TESTING JVM DONE"
}


compare_outs() {
    DIR=$1
    for t in $TESTS; do
        if cmp -s ${DIR}/$t.myout examples/$t.output; then
            echo "$t passed."
        else
            echo "$t NOT passsed!"
        fi
    done;
}

make
for arg in $@; do
    $arg
done;

