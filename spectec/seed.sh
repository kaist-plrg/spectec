(cd ../interpreter && make)
(cd ../../wasmtime && cargo build --release)
mkdir out
make && ./watsup spec/wasm-2.0/*.watsup -l --test --test:seed $1 --test:log 2
