#!/bin/bash

TESTS=`ls -1 examples/ | grep ins | cut -d \. -f 1`
LLVM_DIR=llvm_out

llvm() {

echo "TESTING LLVM..."

for t in $TESTS; do
    `stack run llvm examples/$t.ins > ${LLVM_DIR}/$t.ll`
    `llvm-as ${LLVM_DIR}/$t.ll > ${LLVM_DIR}/$t.bc`
    `lli ${LLVM_DIR}/$t.ll > ${LLVM_DIR}/$t.out`
done;

compare_outs

echo "TESTING LLVM DONE"

}

compare_outs() {
    for t in $TESTS; do
        if cmp -s ${LLVM_DIR}/$t.out examples/$t.output; then
            echo "$t passed."
        else
            echo "$t NOT passsed!"
        fi
    done;
}

mkdir -p ${LLVM_DIR}
llvm

