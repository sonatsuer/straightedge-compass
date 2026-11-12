// Independent checker for the core export (spec/core-format.md).
//
//   node test/check-core.mjs FILE.core.json
//
// It re-checks the hints with ring operations and signs only, as the Lean
// kernel does: radicands are positive, every intersection point lies on both
// objects, every choice condition holds, and every assertion holds. It
// shares no code with the exporter.
import { readFileSync } from "node:fs";

const core = JSON.parse(readFileSync(process.argv[2], "utf8"));
// universal statements have their own checker
if (core.format === "straightedge-compass-universal") await import("./check-universal.mjs");
if (core.format !== "straightedge-compass-core" || core.version !== 1) throw new Error("unsupported core format");
const tower = core.tower; // radicands, bottom up
const D = tower.length;

// Trees in the file are BigInt strings or pairs [a, b] (a + b·√r).
const conv = (t) => (typeof t === "string" ? BigInt(t) : [conv(t[0]), conv(t[1])]);
const isLeaf = (t) => typeof t === "bigint";

// Values are trees of exact rationals (BigInt fractions) at full depth D,
// which keeps this checker obviously correct.
const Q = {
  make: (n, d) => { if (d < 0n) { n = -n; d = -d; } const g = gcd(n < 0n ? -n : n, d); return { n: n / g, d: d / g }; },
  add: (a, b) => Q.make(a.n * b.d + b.n * a.d, a.d * b.d),
  mul: (a, b) => Q.make(a.n * b.n, a.d * b.d),
  neg: (a) => ({ n: -a.n, d: a.d }),
  zero: { n: 0n, d: 1n },
};
function gcd(a, b) { while (b) [a, b] = [b, a % b]; return a || 1n; }

// Values as rational trees at full depth D.
const fromInt = (t, k) => (isLeaf(t) ? embed({ n: t, d: 1n }, k) : [fromInt(t[0], k - 1), fromInt(t[1], k - 1)]);
const embed = (q, k) => (k === 0 ? q : [embed(q, k - 1), embed(Q.zero, k - 1)]);
const scaleQ = (v, q, k) => (k === 0 ? Q.mul(v, q) : [scaleQ(v[0], q, k - 1), scaleQ(v[1], q, k - 1)]);
const elt = (e, k) => scaleQ(fromInt(conv(e.num), k), { n: 1n, d: BigInt(e.den) }, k);
const radicands = tower.map((e, i) => elt(e, i));
const add = (a, b, k) => (k === 0 ? Q.add(a, b) : [add(a[0], b[0], k - 1), add(a[1], b[1], k - 1)]);
const neg = (a, k) => (k === 0 ? Q.neg(a) : [neg(a[0], k - 1), neg(a[1], k - 1)]);
const sub = (a, b, k) => add(a, neg(b, k), k);
const mul = (a, b, k) => {
  if (k === 0) return Q.mul(a, b);
  const r = radicands[k - 1];
  return [add(mul(a[0], b[0], k - 1), mul(r, mul(a[1], b[1], k - 1), k - 1), k - 1), add(mul(a[0], b[1], k - 1), mul(a[1], b[0], k - 1), k - 1)];
};
const sign = (a, k) => {
  if (k === 0) return a.n > 0n ? 1 : a.n < 0n ? -1 : 0;
  const s0 = sign(a[0], k - 1), s1 = sign(a[1], k - 1);
  if (s1 === 0) return s0;
  if (s0 === 0) return s1;
  if (s0 === s1) return s0;
  const r = radicands[k - 1];
  const n = sub(mul(a[0], a[0], k - 1), mul(r, mul(a[1], a[1], k - 1), k - 1), k - 1);
  return s0 > 0 ? sign(n, k - 1) : -sign(n, k - 1);
};
const isZero = (a, k) => sign(a, k) === 0;
const approx = (a, k) => {
  if (k === 0) return Number(a.n) / Number(a.d);
  return approx(a[0], k - 1) + approx(a[1], k - 1) * Math.sqrt(approx(radicands[k - 1], k - 1));
};

let failures = 0;
const fail = (msg) => { failures++; if (failures < 20) console.log("FAIL", msg); };
radicands.forEach((r, i) => { if (sign(r, i) <= 0) fail(`radicand ${i} is not positive`); });

const objs = new Map(core.objects.map((o) => [o.id, o]));
const P = (id) => { const o = objs.get(id); return { x: elt(o.coords.x, D), y: elt(o.coords.y, D) }; };
const ratP = (p) => { const f = (s) => { const [n, d] = s.split("/"); return embed({ n: BigInt(n), d: BigInt(d ?? "1") }, D); }; return { x: f(p.x), y: f(p.y) }; };
const dist2 = (p, q) => { const dx = sub(q.x, p.x, D), dy = sub(q.y, p.y, D); return add(mul(dx, dx, D), mul(dy, dy, D), D); };
const cross = (p, q, r) => sub(mul(sub(q.x, p.x, D), sub(r.y, p.y, D), D), mul(sub(q.y, p.y, D), sub(r.x, p.x, D), D), D);
const onObj = (pt, id) => {
  const o = objs.get(id);
  if (o.kind === "line") return isZero(cross(P(o.def.line[0]), P(o.def.line[1]), pt), D);
  if (o.kind === "circle") return isZero(sub(dist2(pt, P(o.def.circle[0])), dist2(P(o.def.circle[1]), P(o.def.circle[0])), D), D);
  return false;
};
const same = (p, q) => isZero(sub(p.x, q.x, D), D) && isZero(sub(p.y, q.y, D), D);
const refPt = (r) => (r.object !== undefined ? P(r.object) : ratP(r.point));

let checked = 0;
for (const o of core.objects) {
  if (o.kind !== "point") continue;
  const p = P(o.id);
  if (o.def.given) {
    if (!same(p, ratP(o.def.given))) fail(`${o.name}: hint differs from the given value`);
    continue;
  }
  const [a, b] = o.def.meet;
  if (!onObj(p, a) || !onObj(p, b)) fail(`${o.name} is not on both objects`);
  const ch = o.def.choice;
  if (ch.kind !== "unique") {
    const q = { x: elt(o.def.other.x, D), y: elt(o.def.other.y, D) };
    if (!onObj(q, a) || !onObj(q, b)) fail(`${o.name}: the other point is not on both objects`);
    if (same(p, q)) fail(`${o.name}: the two points coincide`);
    const lt = (u, v) => sign(sub(u, v, D), D) < 0;
    const ok = {
      lexFirst: () => lt(p.x, q.x) || (isZero(sub(p.x, q.x, D), D) && lt(p.y, q.y)),
      lexSecond: () => lt(q.x, p.x) || (isZero(sub(p.x, q.x, D), D) && lt(q.y, p.y)),
      except: () => same(q, refPt(ch.ref)),
      nearest: () => lt(dist2(p, refPt(ch.ref)), dist2(q, refPt(ch.ref))),
      farthest: () => lt(dist2(q, refPt(ch.ref)), dist2(p, refPt(ch.ref))),
      left: () => sign(cross(P(ch.line[0]), P(ch.line[1]), p), D) > 0 && sign(cross(P(ch.line[0]), P(ch.line[1]), q), D) <= 0,
      right: () => sign(cross(P(ch.line[0]), P(ch.line[1]), p), D) < 0 && sign(cross(P(ch.line[0]), P(ch.line[1]), q), D) >= 0,
    }[ch.kind];
    if (!ok()) fail(`${o.name}: choice ${ch.kind} does not hold`);
  }
  checked++;
}
// Assertions (the same exact conditions the evaluator uses, without division).
const lineDir = (id) => { const o = objs.get(id); const [a, b] = o.def.line; return { p: P(a), q: P(b) }; };
const dotV = (u, v) => add(mul(u.x, v.x, D), mul(u.y, v.y, D), D);
const crossV = (u, v) => sub(mul(u.x, v.y, D), mul(u.y, v.x, D), D);
const vec = (p, q) => ({ x: sub(q.x, p.x, D), y: sub(q.y, p.y, D) });
const det3 = (m) =>
  add(add(mul(m[0][0], sub(mul(m[1][1], m[2][2], D), mul(m[1][2], m[2][1], D), D), D),
    neg(mul(m[0][1], sub(mul(m[1][0], m[2][2], D), mul(m[1][2], m[2][0], D), D), D), D), D),
    mul(m[0][2], sub(mul(m[1][0], m[2][1], D), mul(m[1][1], m[2][0], D), D), D), D);
// On the circle through a, b, c (non-collinear): det |x²+y², x, y, 1| = 0,
// computed relative to d so that the last column drops out.
const onCircleABC = (a, b, c, d) => {
  const row = (p) => { const v = vec(d, p); return [add(mul(v.x, v.x, D), mul(v.y, v.y, D), D), v.x, v.y]; };
  return isZero(det3([row(a), row(b), row(c)]), D);
};
const checkPred = (pr) => {
  const a = pr.args;
  switch (pr.kind) {
    case "on": return onObj(P(a[0]), a[1]);
    case "eq": return same(P(a[0]), P(a[1]));
    case "ne": return !same(P(a[0]), P(a[1]));
    case "dist": {
      const s = sign(sub(dist2(P(a[0]), P(a[1])), dist2(P(a[2]), P(a[3])), D), D);
      return { "==": s === 0, "!=": s !== 0, "<": s < 0, "<=": s <= 0, ">": s > 0, ">=": s >= 0 }[pr.cmp];
    }
    case "collinear": {
      const ps = a.map(P);
      const q = ps.find((x) => !same(x, ps[0]));
      return !q || ps.every((r) => isZero(cross(ps[0], q, r), D));
    }
    case "concyclic": {
      const ps = a.map(P).filter((x, i, arr) => arr.findIndex((y) => same(x, y)) === i);
      if (ps.length < 3) return true;
      if (isZero(cross(ps[0], ps[1], ps[2]), D)) return false;
      return ps.slice(3).every((d) => onCircleABC(ps[0], ps[1], ps[2], d));
    }
    case "parallel": { const l = lineDir(a[0]), m = lineDir(a[1]); return isZero(crossV(vec(l.p, l.q), vec(m.p, m.q)), D); }
    case "perpendicular": { const l = lineDir(a[0]), m = lineDir(a[1]); return isZero(dotV(vec(l.p, l.q), vec(m.p, m.q)), D); }
    case "angle": {
      const [A, B, C, Dd, E, F] = a.map(P);
      const u1 = vec(B, A), v1 = vec(B, C), u2 = vec(E, Dd), v2 = vec(E, F);
      const d1 = dotV(u1, v1), d2 = dotV(u2, v2);
      const n1 = mul(dotV(u1, u1), dotV(v1, v1), D), n2 = mul(dotV(u2, u2), dotV(v2, v2), D);
      return sign(d1, D) === sign(d2, D) && isZero(sub(mul(mul(d1, d1, D), n2, D), mul(mul(d2, d2, D), n1, D), D), D);
    }
    case "ccw": return sign(cross(P(a[0]), P(a[1]), P(a[2])), D) > 0;
    case "between": {
      const [A, B, C] = a.map(P);
      return isZero(cross(A, B, C), D) && sign(dotV(vec(B, A), vec(B, C)), D) < 0;
    }
    default: throw new Error("unknown predicate " + pr.kind);
  }
};
for (const as of core.asserts) {
  if (!checkPred(as.pred)) fail(`assertion does not hold: ${as.text}`);
  // the centre hinted for a concyclic assertion is at one distance from all points
  if (as.centre) {
    const o = { x: elt(as.centre.x, D), y: elt(as.centre.y, D) };
    const ps = as.pred.args.map(P);
    const r = dist2(ps[0], o);
    if (!ps.every((p) => isZero(sub(dist2(p, o), r, D), D))) fail(`wrong centre hint: ${as.text}`);
  }
}

console.log(`tower depth ${D}, ${core.objects.length} objects, ${checked} intersection points and ${core.asserts.length} assertions checked, ${failures} failures`);
const p1 = core.objects.find((o) => o.name === "P1");
if (p1) console.log("P1 ≈", approx(elt(p1.coords.x, D), D), approx(elt(p1.coords.y, D), D));
process.exit(failures ? 1 : 0);
