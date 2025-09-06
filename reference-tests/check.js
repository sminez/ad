// Read in a patch file and check that the patches all apply correctly.
const fs = require('fs')
const assert = require('assert')
const zlib = require('zlib')

const filename = process.argv[2]
const steps = parseInt(process.argv[3])

if (filename == null) {
  console.error(`Usage: $ node check.js <file.json[.gz]> <steps>`)
  process.exit(1)
}

const {
  startContent,
  endContent,
  txns
} = JSON.parse(
  filename.endsWith('.gz')
  ? zlib.gunzipSync(fs.readFileSync(filename))
  : fs.readFileSync(filename, 'utf-8')
)

let content = startContent

if (!fs.existsSync('output')){
  fs.mkdirSync('output');
}

let lastTime = 0
for (let i = 0; i < txns.length; i++) {
  if (i % 10000 == 0) console.log(i)

  if (steps != null && i === steps) {
      fs.writeFileSync("output/js_output.txt", content + '\n')
      return
  }

  const {time, patches} = txns[i]

  for (const [pos, delHere, insContent] of patches) {
    assert(content.length >= pos + delHere)
    const before = content.slice(0, pos)
    const after = content.slice(pos + delHere)
    content = before + insContent + after
  }
}

assert.strictEqual(content, endContent)
console.log(`Looking good - ${txns.length} apply cleanly.`)
fs.writeFileSync("output/js_output.txt", content)
