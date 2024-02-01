# WasmEdge

## Prerequisites
#### Install WasmEdge
Follow the instructions in the official wasmedge documentation to install the wasmedge library

https://wasmedge.org/docs/start/install#install

The following command allows users to install to a local directory
```bash
curl -sSf https://raw.githubusercontent.com/WasmEdge/WasmEdge/master/utils/install.sh | bash -s -- -p {INSTALL_PATH}
```

## Building
Build tester.c with ``make`` in this directory

## Running
Run .wast files in wasmedge with ``./wasmedge {PATH_TO_WAST_FILES}``
PATH_TO_WAST_FILES can be a directory containing .wast files or a path to a single .wast file
