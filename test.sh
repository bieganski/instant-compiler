#!/bin/bash

# TESTS=`ls -1 examples/ | grep ins | cut -d \. -f 1`
TESTS=test01

llvm() {
echo "TESTING LLVM..."

for t in $TESTS; do
    `stack run insc_llvm examples/$t.ins > ${LLVM_DIR}/$t.ll`
    `llvm-as ${LLVM_DIR}/$t.ll > ${LLVM_DIR}/$t.bc`
    `lli ${LLVM_DIR}/$t.ll > ${LLVM_DIR}/$t.out`
done;

compare_outs $LLVM_DIR

echo "TESTING LLVM DONE"

}

jvm() {
    make
    echo "TESTING JVM..."
    for t in $TESTS; do
        ./insc_jvm examples/$t.ins
        `java examples.$t > $t.myout`
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


for arg in $@; do
    $arg
done;

