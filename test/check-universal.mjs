// Independent checker for the universal export (spec/universal-format.md).
//
//   node test/check-universal.mjs FILE.core.json
//
// For every case it re-checks, as Lean's kernel does (`checkGoals` in
// StraightedgeCompass/Cert.lean): the hypotheses use the parameters only,
// every certificate of the chain is a polynomial identity of the right
// shape, every square root step is admissible, and every goal (computed from
// the points' polynomial coordinates as in StraightedgeCompass/UGeometry.lean)
// is a fact of the chain. It shares no code with the exporter.
import { readFileSync } from "node:fs";

// ---------------------------------------------------------------------------
// Polynomials with BigInt coefficients: Map from exponent key to { e, c }.

const trim = (e) => {
  let k = e.length;
  while (k > 0 && e[k - 1] === 0) k--;
  return e.slice(0, k);
};
const key = (e) => e.join(",");
const P = {
  zero: () => new Map(),
  const: (c) => P.addTerm(new Map(), [], BigInt(c)),
  var: (i) => P.addTerm(new Map(), [...Array(i).fill(0), 1], 1n),
  addTerm(m, e, c) {
    if (c === 0n) return m;
    e = trim(e);
    const k = key(e);
    const t = m.get(k);
    const s = (t ? t.c : 0n) + c;
    if (s === 0n) m.delete(k);
    else m.set(k, { e, c: s });
    return m;
  },
  add(a, b) {
    const m = new Map(a);
    for (const t of b.values()) P.addTerm(m, t.e, t.c);
    return m;
  },
  neg(a) {
    const m = new Map();
    for (const t of a.values()) m.set(key(t.e), { e: t.e, c: -t.c });
    return m;
  },
  sub: (a, b) => P.add(a, P.neg(b)),
  mul(a, b) {
    const m = new Map();
    for (const s of a.values())
      for (const t of b.values()) {
        const n = Math.max(s.e.length, t.e.length);
        const e = [];
        for (let i = 0; i < n; i++) e.push((s.e[i] || 0) + (t.e[i] || 0));
        P.addTerm(m, e, s.c * t.c);
      }
    return m;
  },
  eq(a, b) {
    return P.sub(a, b).size === 0;
  },
  varsBelow: (a, n) => [...a.values()].every((t) => t.e.length <= n),
  isNonzeroConst(a) {
    if (a.size !== 1) return false;
    const [t] = a.values();
    return t.e.length === 0 && t.c !== 0n;
  },
  fromJson(j) {
    const m = new Map();
    for (const [e, c] of j) P.addTerm(m, e, BigInt(c));
    return m;
  },
};

// ---------------------------------------------------------------------------
// Certificates (as Cert.check)

const isPos = (r) => r === "pos";
const isNonneg = (r) => r === "pos" || r === "nonneg";
const isNonzero = (r) => r === "pos" || r === "nonzero";
const isZero = (r) => r === "zero";
const relIs = (K, i, ok) => i < K.length && ok(K[i].rel);
const prod = (K, refs) => refs.reduce((acc, i) => P.mul(acc, i < K.length ? K[i].p : P.zero()), P.const(1));
const termPoly = (K, t) => {
  const sq = prod(K, t.sqrefs);
  return P.mul(P.mul(P.mul(P.const(t.c), prod(K, t.refs)), P.mul(sq, sq)), P.mul(t.sq, t.sq));
};
const termNonneg = (K, t) => t.c > 0n && t.refs.every((i) => relIs(K, i, isNonneg)) && t.sqrefs.every((i) => relIs(K, i, isNonzero));
const termStrict = (K, t) => termNonneg(K, t) && t.refs.every((i) => relIs(K, i, isPos)) && P.isNonzeroConst(t.sq);
const termNonzero = (K, t) => t.c > 0n && t.refs.every((i) => relIs(K, i, isNonzero)) && t.sqrefs.every((i) => relIs(K, i, isNonzero)) && P.isNonzeroConst(t.sq);
const eqsPoly = (K, eqs) => eqs.reduce((acc, e) => P.add(acc, P.mul(e.c, e.ref < K.length ? K[e.ref].p : P.zero())), P.zero());

function checkCert(K, f, c) {
  if (c.sum) {
    const { mult, terms, eqs } = c.sum;
    if (!eqs.every((e) => relIs(K, e.ref, isZero))) return "an equation is not a zero fact";
    if (!terms.every((t) => termNonneg(K, t))) return "a term is not non-negative";
    const lhs = P.mul(termPoly(K, mult), f.p);
    const rhs = P.add(terms.reduce((acc, t) => P.add(acc, termPoly(K, t)), P.zero()), eqsPoly(K, eqs));
    if (!P.eq(lhs, rhs)) return "the identity fails";
    if (f.rel === "zero") return terms.length === 0 && termNonzero(K, mult) ? null : "zero: terms, or the multiplier";
    if (f.rel === "nonneg") return termStrict(K, mult) ? null : "nonneg: the multiplier is not positive";
    return terms.some((t) => termStrict(K, t)) && termNonneg(K, mult) ? null : "no strict term, or the multiplier";
  }
  if (c.nonzero) {
    const { m, c: k, refs, eqs } = c.nonzero;
    if (f.rel !== "nonzero" || k === 0n) return "nonzero: relation or constant";
    if (!eqs.every((e) => relIs(K, e.ref, isZero)) || !refs.every((i) => relIs(K, i, isNonzero))) return "nonzero: refs";
    const rhs = P.add(P.mul(P.const(k), prod(K, refs)), eqsPoly(K, eqs));
    return P.eq(P.mul(f.p, m), rhs) ? null : "nonzero: the identity fails";
  }
  if (c.strict) {
    const [i, j] = c.strict;
    const ok = f.rel === "pos" && i < K.length && j < K.length && P.eq(K[i].p, f.p) && isNonneg(K[i].rel) && P.eq(K[j].p, f.p) && isNonzero(K[j].rel);
    return ok ? null : "strict: the facts do not match";
  }
  return "unknown certificate";
}

// ---------------------------------------------------------------------------
// Points (as UPt)

const U = {
  dxN: (a, b) => P.sub(P.mul(b.x, a.d), P.mul(a.x, b.d)),
  dyN: (a, b) => P.sub(P.mul(b.y, a.d), P.mul(a.y, b.d)),
  dist2N: (a, b) => P.add(P.mul(U.dxN(a, b), U.dxN(a, b)), P.mul(U.dyN(a, b), U.dyN(a, b))),
  crossN: (a, b, x) => P.sub(P.mul(U.dxN(a, b), U.dyN(a, x)), P.mul(U.dyN(a, b), U.dxN(a, x))),
  dotN: (a, b, c) => P.add(P.mul(U.dxN(a, b), U.dxN(a, c)), P.mul(U.dyN(a, b), U.dyN(a, c))),
  circN: (o, a, x) => P.sub(P.mul(U.dist2N(o, x), P.mul(a.d, a.d)), P.mul(U.dist2N(o, a), P.mul(x.d, x.d))),
  cmpN: (a, b, c, d) => {
    const cd = P.mul(c.d, d.d);
    const ab = P.mul(a.d, b.d);
    return P.sub(P.mul(U.dist2N(a, b), P.mul(cd, cd)), P.mul(U.dist2N(c, d), P.mul(ab, ab)));
  },
  leftN: (a, b, x) => P.mul(U.crossN(a, b, x), P.mul(b.d, x.d)),
  lexXN: (x, y) => P.mul(U.dxN(x, y), P.mul(x.d, y.d)),
  lexYN: (x, y) => P.mul(U.dyN(x, y), P.mul(x.d, y.d)),
  crossDirN: (a, b, c, d) => P.sub(P.mul(U.dxN(a, b), U.dyN(c, d)), P.mul(U.dyN(a, b), U.dxN(c, d))),
  dotDirN: (a, b, c, d) => P.add(P.mul(U.dxN(a, b), U.dxN(c, d)), P.mul(U.dyN(a, b), U.dyN(c, d))),
  dotSignN: (b, a, c) => P.mul(U.dotN(b, a, c), P.mul(a.d, c.d)),
  angleN: (a, b, c, d, e, f) =>
    P.sub(
      P.mul(P.mul(U.dotN(b, a, c), U.dotN(b, a, c)), P.mul(U.dist2N(e, d), U.dist2N(e, f))),
      P.mul(P.mul(U.dotN(e, d, f), U.dotN(e, d, f)), P.mul(U.dist2N(b, a), U.dist2N(b, c))),
    ),
  alongU: (a, b, t) => ({ x: P.add(P.mul(a.x, b.d), P.mul(t, U.dxN(a, b))), y: P.add(P.mul(a.y, b.d), P.mul(t, U.dyN(a, b))), d: P.mul(a.d, b.d) }),
  rotU(o, b, t) {
    const tt = P.mul(t, t);
    const one = P.const(1);
    const two = P.const(2);
    const u = U.dxN(o, b);
    const w = U.dyN(o, b);
    return {
      x: P.add(P.mul(P.mul(o.x, b.d), P.add(one, tt)), P.sub(P.mul(P.sub(one, tt), u), P.mul(P.mul(two, t), w))),
      y: P.add(P.mul(P.mul(o.y, b.d), P.add(one, tt)), P.add(P.mul(P.mul(two, t), u), P.mul(P.sub(one, tt), w))),
      d: P.mul(P.mul(o.d, b.d), P.add(one, tt)),
    };
  },
};

// ---------------------------------------------------------------------------

const fact = (f) => ({ p: P.fromJson(f.p), rel: f.rel });
const term = (t) => ({ c: BigInt(t.c), refs: t.refs, sqrefs: t.sqrefs, sq: P.fromJson(t.sq) });
const eqT = (e) => ({ c: P.fromJson(e.c), ref: e.ref });

// Runs the steps of a chain on the facts K (extended in place); null or an error.
function runChain(K, steps, S, nv0) {
  let nv = nv0;
  let si = 0;
  for (const [i, st] of steps.entries()) {
    if (st.sqrt) {
      const q = st.sqrt;
      const n = P.fromJson(q.n);
      const d = P.fromJson(q.d);
      if (si >= S.length || !P.eq(n, S[si][0]) || !P.eq(d, S[si][1])) return `step ${i}: the square root does not match`;
      if (!P.varsBelow(n, nv) || !P.varsBelow(d, nv)) return `step ${i}: the radicand uses later roots`;
      const rad = K[q.rad];
      const den = K[q.den];
      if (!rad || !P.eq(rad.p, P.mul(n, d)) || !isNonneg(rad.rel)) return `step ${i}: the radicand fact`;
      if (!den || !P.eq(den.p, d) || !isNonzero(den.rel)) return `step ${i}: the denominator fact`;
      const s = P.var(nv);
      K.push({ p: P.sub(P.mul(d, P.mul(s, s)), n), rel: "zero" });
      K.push({ p: s, rel: isPos(rad.rel) ? "pos" : "nonneg" });
      nv++;
      si++;
      continue;
    }
    const f = fact(st.fact);
    const c = st.cert;
    const cert = c.sum
      ? { sum: { mult: term(c.sum.mult), terms: c.sum.terms.map(term), eqs: c.sum.eqs.map(eqT) } }
      : c.nonzero
        ? { nonzero: { m: P.fromJson(c.nonzero.m), c: BigInt(c.nonzero.c), refs: c.nonzero.refs, eqs: c.nonzero.eqs.map(eqT) } }
        : { strict: c.strict };
    const err = checkCert(K, f, cert);
    if (err) return `step ${i} (fact ${K.length}): ${err}`;
    K.push(f);
  }
  if (si !== S.length) return "square roots left over";
  return null;
}

const gives = (f, g) => f && P.eq(f.p, g.p) && (f.rel === g.rel || (f.rel === "pos" && (g.rel === "nonneg" || g.rel === "nonzero")));

function checkLeaf(leaf) {
  const ch = leaf.chain;
  const H = ch.hyps.map(fact);
  if (!H.every((f) => P.varsBelow(f.p, ch.nparams))) return "a hypothesis uses a square root";
  const S = ch.sqrts.map(([n, d]) => [P.fromJson(n), P.fromJson(d)]);
  const K = [...H];
  const err = runChain(K, ch.steps, S, ch.nparams);
  if (err) return err;
  // the points
  const upts = new Map();
  const uptOf = (d) => {
    if (d.along) return U.alongU(upts.get(d.along[0]), upts.get(d.along[1]), P.var(d.t));
    if (d.rot) return U.rotU(upts.get(d.rot[0]), upts.get(d.rot[1]), P.var(d.t));
    return { x: P.fromJson(d.x), y: P.fromJson(d.y), d: P.fromJson(d.d) };
  };
  for (const u of leaf.upts) upts.set(u.name, uptOf(u.def));
  for (const c of leaf.conjuncts) if (c.kind === "concyclic") upts.set(c.centre, uptOf(c.centreDef));
  // the goals
  for (const [k, g] of leaf.goals.entries()) {
    const args = g.args.map((a) => upts.get(a));
    if (args.some((a) => !a)) return `goal ${k}: unknown point`;
    let p = g.fn === "d" ? args[0].d : U[g.fn](...args);
    if (g.neg === "yes") p = P.neg(p);
    if (!gives(K[g.index], { p, rel: g.rel })) return `goal ${k} (${g.fn} ${g.args.join(" ")} ${g.rel}): not fact ${g.index} of the chain`;
  }
  return null;
}

// A coverage tree (as checkCover): every branch ends in a case or a contradiction.
function checkCover(cv) {
  const D = cv.D.map((d) => d.map(fact));
  const K = cv.facts.map(fact);
  const err = runChain(K, cv.root, [], cv.nparams);
  if (err) return "root: " + err;
  const go = (K, t, path) => {
    if (t.split) {
      const p = P.fromJson(t.split);
      return (
        go([...K, { p: P.neg(p), rel: "pos" }], t.neg, path + "<") ||
        go([...K, { p, rel: "zero" }], t.zero, path + "=") ||
        go([...K, { p, rel: "pos" }], t.pos, path + ">")
      );
    }
    if (t.chain) {
      const K1 = [...K];
      const e = runChain(K1, t.chain, [], cv.nparams);
      return e ? `${path}: ${e}` : go(K1, t.next, path + "·");
    }
    const K2 = [...K];
    const e = runChain(K2, t.steps, [], cv.nparams);
    if (e) return `${path}: ${e}`;
    if (t.case !== undefined) {
      const d = D[t.case];
      if (!d || d.length !== t.idx.length) return `${path}: case ${t.case} does not match`;
      for (const [k, g] of d.entries()) if (!gives(K2[t.idx[k]], g)) return `${path}: condition ${k} of case ${t.case} is not fact ${t.idx[k]}`;
      return null;
    }
    const [i, j] = t.absurd;
    const f = K2[i];
    const g = K2[j];
    const ok = f && g && ((f.rel === "zero" && isNonzero(g.rel) && P.eq(f.p, g.p)) || (f.rel === "pos" && isNonneg(g.rel) && P.eq(g.p, P.neg(f.p))));
    return ok ? null : `${path}: facts ${i} and ${j} do not contradict`;
  };
  return go(K, cv.tree, "");
}

const core = JSON.parse(readFileSync(process.argv[2], "utf8"));
if (core.format !== "straightedge-compass-universal" || core.version !== 1) throw new Error("unsupported universal format");
let failed = false;
for (const leaf of core.leaves) {
  const err = checkLeaf(leaf);
  const name = leaf.name || "(the theorem)";
  if (err) {
    console.error(`${process.argv[2]}: ${name}: ${err}`);
    failed = true;
  }
}
for (const cv of core.covers || []) {
  const err = checkCover(cv);
  if (err) {
    console.error(`${process.argv[2]}: ${cv.name}: ${err}`);
    failed = true;
  }
}
const nc = (core.covers || []).length;
if (!failed) console.log(`${process.argv[2]}: ${core.leaves.length} case${core.leaves.length === 1 ? "" : "s"}${nc ? `, ${nc} coverage tree${nc === 1 ? "" : "s"}` : ""} checked`);
process.exit(failed ? 1 : 0);
