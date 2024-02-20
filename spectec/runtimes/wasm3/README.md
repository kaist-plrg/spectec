# Wasm3

## Prerequisites
Running Wasm3 with .wast file requires ``wast2json`` and ``wasm3``.

### wast2json
Clone and build [wabt](https://github.com/WebAssembly/wabt/tree/main).  
Make sure executable ``wast2json`` file is built. If its name is different, change it to ``wast2json``.  
Add the directory ``bin`` (or anywhere with executable ``wast2json`` file inside) to the environment variable $PATH.

### wasm3
Clone and build [Wasm3](https://github.com/wasm3/wasm3), referring to [Wasm3 development notes](https://github.com/wasm3/wasm3/blob/main/docs/Development.md).
Change the file name to ``wasm3`` and locate it into this directory.

## Running
Usage: ``./run.sh [-o] [-w {WORD}] [-v] {PATH_TO_WAST_DIRECTORY}``

PATH_TO_WAST_FILES can be a directory containing .wast files or a path to a single .wast file.

Argument ``-o`` enables you to get output files in ``wasm3/temp`` directory.  
Argument ``-w`` enables you to run only .wast files whose name contains WORD (works only with directory).  
Argument ``-v`` enables you to run in verbose mode.
