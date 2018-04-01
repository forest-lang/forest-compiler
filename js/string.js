const fs = require('fs');

const bytes = fs.readFileSync('./output.wasm');

WebAssembly.instantiate(bytes).then(m => {
  const exports = m.instance.exports;

  const address = exports.main();

  const buffer = new Uint8Array(exports.memory.buffer);

  let output = '';

  let length = buffer[address] - 1;
  console.log('address', address);
  console.log('length', length);

  for(let i = 1; i <= length; i++) {
    console.log(address + i, buffer[address + i]);
    output += String.fromCodePoint(buffer[address + i]);
  }

  console.log(output);
});

