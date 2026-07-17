import { rm } from "node:fs/promises";
import { build, context } from "esbuild";

const watch = process.argv.includes("--watch");
const production = process.argv.includes("--production");

/** @type {import("esbuild").BuildOptions} */
const options = {
  entryPoints: ["src/extension.ts"],
  outfile: "dist/extension.js",
  bundle: true,
  format: "cjs",
  platform: "node",
  target: "node20",
  external: ["vscode"],
  sourcemap: true,
  minify: production,
  logLevel: "info"
};

await rm("dist", {recursive: true, force: true});

if (watch) {
  const running = await context(options);
  await running.watch();
  console.log("Watching src/ for changes; press Ctrl-C to stop.");
} else {
  await build(options);
}
