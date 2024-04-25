# V8

## Prerequisites
Running V8 with .wast file requires reference interpreter and installation of ``d8``(V8’s own developer shell).

### Reference interpreter
The shell script ``run.sh`` refers to reference interpreters in the directory ``../util/interpreter`` to convert the input ``.wast`` file into ``.js`` file. [This document](https://github.com/kaist-plrg/spectec/blob/west/spectec/runtimes/util/interpreter/README.md) explains how to install Wasm reference interpreters.

You may modify the variable ``interpreters`` in ``run.sh`` to decide which versions of reference interpreters will be used for conversion of ``.wast`` files.

### d8
Clone and build [V8](https://v8.dev/), referring to V8 documents([Checking out the V8 source code](https://v8.dev/docs/source-code), [Building V8 from source](https://v8.dev/docs/build)).
In ``run.sh``, set the variable ``D8_PATH`` as the path to ``d8`` file.

## Running
Usage: ``./run.sh [-o] [-w {WORD}] [-v] {PATH_TO_WAST_DIRECTORY}``

PATH_TO_WAST_FILES can be a directory containing .wast files or a path to a single .wast file.

Argument ``-o`` enables you to get output files in ``v8/temp`` directory.  
Argument ``-w`` enables you to run only .wast files whose name contains WORD (works only with directory).  
Argument ``-v`` enables you to run in verbose mode.
