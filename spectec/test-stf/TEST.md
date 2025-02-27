# Preview

```sh
$ ../src/exe-watsup/main.exe ../spec/wasm-3.0/*.watsup -v -l --stf
$ (cd $SPECTEC_HOME/interpreter && make)
rm -f wasm
dune build wasm.exe
ln _build/default/wasm.exe wasm
$ for file in wasm/*; do ( \
>   echo run $(basename $file) && \
>   $SPECTEC_HOME/interpreter/wasm $file \
> ) done
$ for file in template/*; do ( \
>   echo run $(basename $file) && \
>   $V8_HOME/out/x64.release/d8 $file \
> ) done
```
