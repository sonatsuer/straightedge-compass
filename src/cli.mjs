#!/usr/bin/env node
// Command-line interface over the compiled PureScript modules (run
// `spago build` first).
//
//   node src/cli.mjs check FILE.sc...            check scripts (exit 1 on errors)
//   node src/cli.mjs pdf FILE.sc [OUT.pdf]
//   node src/cli.mjs typst FILE.sc [OUT.typ]
//   node src/cli.mjs core FILE.sc... [--out-dir DIR]   Lean core export (FILE.core.json)

import { readFileSync, writeFileSync } from "node:fs";
import { createRequire } from "node:module";
import path from "node:path";

const out = (m) => new URL(`../output/${m}/index.js`, import.meta.url).href;
const Eval = await import(out("Proof.Eval"));
const Typst = await import(out("Export.Typst"));
const Core = await import(out("Export.Core"));
const Prove = await import(out("Proof.Prove"));
const Parser = await import(out("Proof.Parser"));
const TypstParam = await import(out("Export.TypstParam"));
const Universal = await import(out("Export.Universal"));

// The proof of a script with variables: { script, proof }.
function proofOf(file) {
  const src = readFileSync(file, "utf8");
  return { script: Parser.parseScript(src).value0, proof: Prove.proveSource(src).value0 };
}

// Counts the cases and the proved claims of a proof tree.
function census(node) {
  let leaves = 0;
  let claims = 0;
  const go = (n) => {
    claims += n.steps.reduce((k, s) => k + s.verdicts.filter((v) => v.holds && s.kind.constructor.name === "SAssert").length, 0);
    if (n.split.constructor.name === "Just") {
      for (const b of n.split.value0.branches) for (const c of b.cases) if (c.node.constructor.name === "Just") go(c.node.value0);
    } else leaves++;
  };
  go(node);
  return { leaves, claims };
}

const usage = `usage:
  node src/cli.mjs check FILE.sc...
  node src/cli.mjs pdf FILE.sc [OUT.pdf]
  node src/cli.mjs typst FILE.sc [OUT.typ]
  node src/cli.mjs core FILE.sc... [--out-dir DIR]`;

const args = process.argv.slice(2);
const option = (name) => {
  const i = args.indexOf(name);
  if (i < 0 || i + 1 >= args.length) return null;
  const v = args[i + 1];
  args.splice(i, 2);
  return v;
};

const [command, ...rest] = args;

// Parses and evaluates a script; returns the trace or prints the error.
function load(file) {
  const result = Eval.evaluateSource(readFileSync(file, "utf8"));
  if (result.constructor.name === "Left") {
    const e = result.value0;
    console.error(`${file}:${e.line}:${e.column}: parse error: ${e.message}`);
    return null;
  }
  const trace = result.value0;
  if (trace.error.constructor.name === "Just") {
    const e = trace.error.value0;
    const context = e.context.length ? ` (${e.context.join(", ")})` : "";
    console.error(`${file}:${e.line}: ${e.message}${context}`);
    return null;
  }
  return trace;
}

const replaceExt = (file, ext) => file.replace(/\.sc$/, "") + ext;

switch (command) {
  case "check": {
    if (rest.length === 0) throw new Error(usage);
    let failed = 0;
    for (const file of rest) {
      const t0 = performance.now();
      const trace = load(file);
      if (!trace) {
        failed++;
        continue;
      }
      if (trace.parametric) {
        const { proof } = proofOf(file);
        const ms = (performance.now() - t0).toFixed(0);
        if (proof.errors.length) {
          for (const e of proof.errors) console.error(`${file}:${e.line}: ${e.message}`);
          failed++;
          continue;
        }
        const c = census(proof.root);
        console.log(`${file}: universal, ${c.leaves} case${c.leaves === 1 ? "" : "s"}, ${c.claims} claims proved (${ms} ms)`);
        continue;
      }
      const ms = (performance.now() - t0).toFixed(0);
      console.log(`${file}: ${trace.steps.length} steps, ${trace.asserts.length} checks hold (${ms} ms)`);
    }
    process.exit(failed ? 1 : 0);
  }
  case "typst":
  case "pdf": {
    const [file, target] = rest;
    if (!file) throw new Error(usage);
    const trace = load(file);
    if (!trace) process.exit(1);
    let source;
    if (trace.parametric) {
      const { script, proof } = proofOf(file);
      if (proof.errors.length) {
        for (const e of proof.errors) console.error(`${file}:${e.line}: ${e.message}`);
        process.exit(1);
      }
      source = TypstParam.document(script)(proof);
    } else source = Typst.document(trace);
    if (command === "typst") {
      const dest = target ?? replaceExt(file, ".typ");
      writeFileSync(dest, source);
      console.log(`wrote ${dest}`);
      break;
    }
    const require = createRequire(import.meta.url);
    const { NodeCompiler } = require("@myriaddreamin/typst-ts-node-compiler");
    const compiler = NodeCompiler.create();
    const doc = compiler.compile({ mainFileContent: source });
    const error = doc.takeError();
    if (error) {
      compiler.fetchDiagnostics(error).forEach((d) => console.error(d.message ?? JSON.stringify(d)));
      process.exit(1);
    }
    const dest = target ?? replaceExt(file, ".pdf");
    writeFileSync(dest, compiler.pdf(doc.result));
    console.log(`wrote ${dest}`);
    break;
  }
  case "core": {
    const outDir = option("--out-dir");
    if (rest.length === 0) throw new Error(usage);
    let failed = 0;
    for (const file of args.slice(1)) {
      const trace = load(file);
      if (!trace) {
        failed++;
        continue;
      }
      const base = path.basename(replaceExt(file, ".core.json"));
      const dest = outDir ? path.join(outDir, base) : replaceExt(file, ".core.json");
      if (trace.parametric) {
        const { proof } = proofOf(file);
        if (proof.errors.length) {
          for (const e of proof.errors) console.error(`${file}:${e.line}: ${e.message}`);
          failed++;
          continue;
        }
        const t0 = performance.now();
        const doc = Universal.document(proof);
        if (doc.constructor.name === "Left") {
          console.error(`${file}: the Lean export failed: ${doc.value0}`);
          failed++;
          continue;
        }
        writeFileSync(dest, doc.value0 + "\n");
        console.log(`wrote ${dest} (universal, ${(performance.now() - t0).toFixed(0)} ms)`);
        continue;
      }
      writeFileSync(dest, Core.document(trace) + "\n");
      console.log(`wrote ${dest}`);
    }
    process.exit(failed ? 1 : 0);
  }
  default:
    console.error(usage);
    process.exit(2);
}
