const esbuild = require('esbuild')
const ElmPlugin = require('esbuild-plugin-elm')
const CopyPlugin = require('esbuild-plugin-copy')

esbuild.build({
  entryPoints: ['src/index.js'],
  bundle: true,
  outdir: 'dist',
  watch: process.argv.includes('--watch'),
  plugins: [
    ElmPlugin({
      debug: process.argv.includes('--watch'),
      clearOnWatch: true,
    }),
    CopyPlugin.default({
      assets: {
        from: ["./src/index.html", "./src/*.css"],
        to: ["./"]
      }
    })
  ],
}).catch(_e => {
  console.log(e)
  return process.exit(1)
})