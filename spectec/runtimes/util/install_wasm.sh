#!/bin/bash

###############################################################################
# Install wasm
###############################################################################

CURRENT_DIR=$(dirname $0)
echo $CURRENT_DIR

mkdir -p /home/WebAssembly
cd /home/WebAssembly
git clone https://github.com/WebAssembly/spec
cd spec/interpreter
eval $(opam env)
make distclean wasm
INTERPRETER_DIR=$CURRENT_DIR/interpreter
mkdir -p $INTERPRETER_DIR
cp /home/WebAssembly/spec/interpreter/wasm $INTERPRETER_DIR

###############################################################################
# Install wasm with proposal support.
###############################################################################

# repos='exception-handling js-types tail-call memory64 extended-const multi-memory function-references gc'
repos='tail-call extended-const function-references gc multi-memory threads relaxed-simd'

for repo in ${repos}; do
  cd /home/WebAssembly
  git clone https://github.com/WebAssembly/${repo}
  cd ${repo}/interpreter
  make clean wasm
  cp /home/WebAssembly/${repo}/interpreter/wasm $INTERPRETER_DIR/wasm-${repo}
done