async function load(name, obj = {}) {
  const buf = read(name, 'binary');
  const module = await WebAssembly.instantiate(buf, obj);
  return module.instance;
}

async function main() {
  const instance1 = await load("wasm/tag-subtype-export.wasm");

  const { tag } = instance1.exports;

  const instance2 = await load("wasm/tag-subtype-import.wasm", {"export" : {tag}});
}

main() // .catch(err => console.log(err));
