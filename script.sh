#!/bin/bash

make
./microc.native hello.mc > hello.ll
echo "Generating llvm code..."
cat hello.ll
llc hello.ll > hello.s
sleep 3
echo ""
echo "Generating assembly..."
cat hello.s
cc -o hello.out hello.s
sleep 3
echo ""
echo "Compiling assembly with C compiler assembly..."
sleep 3
echo ""
echo "Output:"
./hello.out

