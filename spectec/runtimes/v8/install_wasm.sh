#!/bin/bash

###############################################################################
# Install wasm
###############################################################################

mkdir -p /root/WebAssembly
cd /root/WebAssembly
git clone https://github.com/WebAssembly/spec
cd spec/interpreter
eval $(opam env)
make distclean wasm
mkdir -p /home/spectec/spectec/runtimes/v8/interpreter
cp /root/WebAssembly/spec/interpreter/wasm /home/spectec/spectec/runtimes/v8/interpreter

###############################################################################
# Install wasm with proposal support.
###############################################################################

# repos='exception-handling js-types tail-call memory64 extended-const multi-memory function-references gc'
repos='tail-call extended-const function-references gc multi-memory threads relaxed-simd'

for repo in ${repos}; do
  cd /root/WebAssembly
  git clone https://github.com/WebAssembly/${repo}
  cd ${repo}/interpreter
  make clean wasm
  cp /root/WebAssembly/${repo}/interpreter/wasm /home/spectec/spectec/runtimes/v8/interpreter/wasm-${repo}
done