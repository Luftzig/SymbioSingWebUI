const esbuild = require('esbuild')

const ElmPlugin = require('esbuild-plugin-elm')
const CopyPlugin = require('esbuild-plugin-copy')

const buildOptions = {
  entryPoints: ['src/index.js'],
  bundle: true,
  outdir: 'dist',
  plugins: [
    ElmPlugin({
      debug: true,
      clearOnWatch: false,
    }),
    CopyPlugin.default({
      assets: {
        from: ["./src/index.html", "./src/*.css"],
        to: ["./"]
      }
    })
  ],
}

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
