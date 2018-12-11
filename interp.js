
const wasm = require('fs').readFileSync('./output.wasm');

WebAssembly.instantiate(wasm).then(m => {
  console.log(m.instance.exports.main())
  console.log(
    Array.from(
      new Uint32Array(m.instance.exports.memory.buffer)
    ).slice(0, 32).join(' ')
  );
});
