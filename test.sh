#!/bin/bash

# TESTS=`ls -1 examples/ | grep ins | cut -d \. -f 1`
TESTS=test01
LLVM_DIR=llvm_out
JVM_DIR=jvm_out

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
echo "TESTING JVM..."

for t in $TESTS; do
    `stack run insc_jvm examples/$t.ins > ${JVM_DIR}/$t.j`
    `java -jar lib/jasmin.jar ${JVM_DIR}/$t.j -d ${JVM_DIR}/`
    `java ${JVM_DIR}/$t.class > ${JVM_DIR}/$t.out`
done;

compare_outs ${JVM_DIR}

echo "TESTING JVM DONE"

}


compare_outs() {
    DIR=$1
    for t in $TESTS; do
        if cmp -s ${DIR}/$t.out examples/$t.output; then
            echo "$t passed."
        else
            echo "$t NOT passsed!"
        fi
    done;
}


mkdir -p ${LLVM_DIR}
mkdir -p ${JVM_DIR}

for arg in $@; do
    $arg
done;
