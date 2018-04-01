const fs = require('fs');

const bytes = fs.readFileSync('./output.wasm');

WebAssembly.instantiate(bytes).then(m => {
  const exports = m.instance.exports;

  const address = exports.main();

  const buffer = new Uint8Array(exports.memory.buffer);

  let output = '';

  let length = buffer[address] - 1;

  for(let i = 1; i <= length; i++) {
    output += String.fromCodePoint(buffer[address + i]);
  }

  console.log(output);
});

