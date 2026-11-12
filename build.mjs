// Bundles the compiled PureScript (output/) into a static site.
//
//   node build.mjs            build into dist/
//   node build.mjs --watch    rebuild on change and serve dist/ on port 3000
//   node build.mjs --release  minified build into docs/ (served by GitHub Pages)
//
// The PureScript sources must be compiled first (`spago build`); in watch
// mode this script reruns `spago build` itself when a .purs file changes.

import * as esbuild from "esbuild";
import { spawn } from "node:child_process";
import { cp, mkdir, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { watch } from "node:fs";
import path from "node:path";

const args = new Set(process.argv.slice(2));
const release = args.has("--release");
const watchMode = args.has("--watch");
const outdir = release ? "docs" : "dist";

// Every example is a folder examples/<name>/ with a script <name>.sc. The
// scripts are served next to the page, with an index for the Examples menu.
async function copyExamples() {
  const index = [];
  for (const dir of (await readdir("examples", { withFileTypes: true })).sort((a, b) =>
    a.name.localeCompare(b.name),
  )) {
    if (!dir.isDirectory()) continue;
    const script = path.join("examples", dir.name, `${dir.name}.sc`);
    let text;
    try {
      text = await readFile(script, "utf8");
    } catch {
      continue;
    }
    await mkdir(path.join(outdir, "examples", dir.name), { recursive: true });
    await cp(script, path.join(outdir, "examples", dir.name, `${dir.name}.sc`));
    const title = /^\s*title\s+"((?:[^"\\]|\\.)*)"/m.exec(text);
    index.push({ path: `${dir.name}/${dir.name}.sc`, title: title ? title[1] : dir.name });
  }
  await writeFile(path.join(outdir, "examples", "index.json"), JSON.stringify(index, null, 1) + "\n");
}

async function copyStatic() {
  await mkdir(outdir, { recursive: true });
  await cp("static", outdir, { recursive: true });
  await copyExamples();
  // Stop GitHub Pages from running the site through Jekyll.
  await writeFile(path.join(outdir, ".nojekyll"), "");
}

// Code splitting keeps the PDF exporter (typst.ts) out of the main bundle:
// it is a separate chunk, loaded on the first export.
const options = {
  entryPoints: { index: "src/index.js" },
  bundle: true,
  splitting: true,
  format: "esm",
  platform: "browser",
  target: "es2022",
  outdir,
  chunkNames: "chunks/[name]-[hash]",
  // typst.ts can also render in the page; we only compile to PDF.
  external: ["@myriaddreamin/typst-ts-renderer"],
  minify: release,
  sourcemap: !release,
  logLevel: "info",
};

function spagoBuild() {
  return new Promise((resolve) => {
    const child = spawn("npx", ["spago", "build"], { stdio: "inherit" });
    child.on("exit", (code) => resolve(code === 0));
  });
}

if (!watchMode) {
  await rm(outdir, { recursive: true, force: true });
}
await copyStatic();

if (watchMode) {
  const ctx = await esbuild.context(options);
  await ctx.watch();
  const { port } = await ctx.serve({ servedir: outdir, port: 3000 });
  console.log(`Serving ${outdir}/ on http://localhost:${port}`);

  let pending = null;
  const rebuild = () => {
    clearTimeout(pending);
    pending = setTimeout(async () => {
      await spagoBuild();
      await copyStatic();
    }, 100);
  };
  watch("src", { recursive: true }, (_event, file) => {
    if (file && file.endsWith(".purs")) rebuild();
  });
  watch("static", { recursive: true }, rebuild);
  watch("examples", { recursive: true }, rebuild);
} else {
  await esbuild.build(options);
}
