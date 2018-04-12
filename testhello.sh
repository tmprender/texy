#!/bin/bash

make
./texy.native test-hello.texy > test-hello.ll
echo "Generating llvm code..."
cat test-hello.ll
llc test-hello.ll > test-hello.s
sleep 3
echo ""
echo "Generating assembly..."
cat test-hello.s
cc -o test-hello.out test-hello.s
sleep 3
echo ""
echo "Compiling assembly with C compiler assembly..."
sleep 3
echo ""
echo "Output:"
./test-hello.out

