const esbuild = require('esbuild')
const buildOptions = require("./build")

esbuild.serve({
    servedir: buildOptions.outdir,
    port: 8080
  },
  {...buildOptions}
).then(({host, port}) => {
  console.log(`Serving on ${host}:${port}`)
}).catch(e => {
  console.error("Failed to start server", e)
  return process.exit(1)
})
