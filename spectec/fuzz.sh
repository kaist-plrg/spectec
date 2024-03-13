(cd ../interpreter && make)
mkdir out
make && ./watsup spec/wasm-2.0/*.watsup -l --test --test:n $1 --test:log 2 &> log
