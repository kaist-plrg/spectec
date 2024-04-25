# JavaScriptCore

## Prerequisites
Running JavaScriptCore with .wast file requires reference interpreter and installation of ``JavaScriptCore``.

### Reference interpreter
The shell script ``run.sh`` refers to every reference interpreter in the directory ``../util/interpreter`` to convert the input ``.wast`` file into ``.js`` file. [This document](https://github.com/kaist-plrg/spectec/blob/west/spectec/runtimes/util/interpreter/README.md) explains how to install Wasm reference interpreters.

You may modify the variable ``interpreters`` in ``run.sh`` to decide which versions of reference interpreters will be used for conversion of ``.wast`` files.

### JavaScriptCore
Clone and build [JavaScriptCore](https://trac.webkit.org/wiki/JSCOnly) referring to [document](https://trac.webkit.org/wiki/JSCOnly).
In ``run.sh``, set the variable ``JSC_PATH`` as the path to ``jsc`` file.

## Running
Usage: ``./run.sh [-o] [-w {WORD}] [-v] {PATH_TO_WAST_DIRECTORY}``

PATH_TO_WAST_FILES can be a directory containing .wast files or a path to a single .wast file.
1
Argument ``-o`` enables you to get output files in ``v8/temp`` directory.  
Argument ``-w`` enables you to run only .wast files whose name contains WORD (works only with directory).  
Argument ``-v`` enables you to run in verbose mode.
