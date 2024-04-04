# V8

## Prerequisites
Running V8 with .wast file requires reference interpreter and ``d8``(V8’s own developer shell).

### Reference interpreter
Clone and build [Wasm reference interpreter](https://github.com/WebAssembly/spec) referring to [document](https://github.com/WebAssembly/spec/tree/main/interpreter).
Locate the interpreter ``wasm``, which is generally built inside ``spec/interpreter``, into this directory.

### d8
Clone and build [V8](https://v8.dev/), referring to V8 documents([Checking out the V8 source code](https://v8.dev/docs/source-code), [Building V8 from source](https://v8.dev/docs/build)).
In ``run.sh``, set the variable ``D8_PATH`` as the path to ``d8`` file.

## Running
Usage: ``./run.sh [-o] [-w {WORD}] [-v] {PATH_TO_WAST_DIRECTORY}``

PATH_TO_WAST_FILES can be a directory containing .wast files or a path to a single .wast file.

Argument ``-o`` enables you to get output files in ``v8/temp`` directory.  
Argument ``-w`` enables you to run only .wast files whose name contains WORD (works only with directory).  
Argument ``-v`` enables you to run in verbose mode.
