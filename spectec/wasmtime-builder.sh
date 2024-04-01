(cd /home/wasmtime && git checkout v$1 && git submodule update --init && cargo build --release)
