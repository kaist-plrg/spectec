async function load(name, obj = {}) {
  const buf = read(name, 'binary');
  const module = await WebAssembly.instantiate(buf, obj);
  return module.instance;
}

async function main() {
  load("wasm/ref-cast.wasm");
}

main() // .catch(err => console.log(err));
