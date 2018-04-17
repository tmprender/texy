#!/bin/bash

make
./texy.native test-word.texy > test-word.ll
echo "Generating llvm code..."
cat test-word.ll
llc test-word.ll > test-word.s
sleep 3
echo ""
echo "Generating assembly..."
cat test-word.s
cc -o test-word.out test-word.s
sleep 3
echo ""
echo "Compiling assembly with C compiler assembly..."
sleep 3
echo ""
echo "Output:"
./test-word.out

