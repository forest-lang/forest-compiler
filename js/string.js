const fs = require('fs');

const bytes = fs.readFileSync('./output.wasm');

function loadString(exports, str) {
  const buffer = new Uint8Array(exports.memory.buffer);

  const address = exports.malloc(str.length + 1);

  buffer[address] = str.length + 1;

  for(let i = 0; i <= str.length; i++) {
    buffer[address + 1 + i] = str.codePointAt(i);
  }

  return address;
}

function readString(exports, address) {
  const buffer = new Uint8Array(exports.memory.buffer);

  let output = '';

  let length = buffer[address] - 1;

  for(let i = 1; i <= length; i++) {
    output += String.fromCodePoint(buffer[address + i]);
  }

  return output;
}

WebAssembly.instantiate(bytes).then(m => {
  const exports = m.instance.exports;

  const string = s => loadString(exports, s);

  const name = process.argv[2];

  const address = exports.main(string(name));

  console.log(readString(exports, address));
});

