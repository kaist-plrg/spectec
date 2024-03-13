(cd ../interpreter && make)
make && ./watsup spec/wasm-2.0/*.watsup -l --test --test:seed $1 --test:log 2
