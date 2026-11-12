// Sparse multivariate polynomials over the rationals, on native BigInt.
//
// A polynomial is a Map from a monomial key to a term { e, n, d }: `e` is the
// exponent array (index = variable, trailing zeros trimmed), and n/d is the
// coefficient in lowest terms with d > 0. Terms are never zero. Polynomials
// are immutable: every operation returns a new Map.
//
// Monomials are ordered lexicographically with the HIGHEST variable index
// most significant, so the "main variable" of a polynomial is its largest
// variable index. The gcd is a recursive primitive PRS over that order.

// ---------------------------------------------------------------------------
// Rationals

const igcd = (a, b) => {
  if (a < 0n) a = -a;
  if (b < 0n) b = -b;
  while (b !== 0n) {
    const t = a % b;
    a = b;
    b = t;
  }
  return a;
};

const rat = (n, d) => {
  if (d === 0n) throw new RangeError("Poly: division by zero");
  if (d < 0n) {
    n = -n;
    d = -d;
  }
  if (d === 1n) return { n, d };
  const g = igcd(n, d);
  return g === 1n ? { n, d } : { n: n / g, d: d / g };
};

const RZERO = { n: 0n, d: 1n };
const RONE = { n: 1n, d: 1n };
const radd = (a, b) => (a.d === 1n && b.d === 1n ? { n: a.n + b.n, d: 1n } : rat(a.n * b.d + b.n * a.d, a.d * b.d));
const rsub = (a, b) => radd(a, { n: -b.n, d: b.d });
const rmul = (a, b) => (a.d === 1n && b.d === 1n ? { n: a.n * b.n, d: 1n } : rat(a.n * b.n, a.d * b.d));
const rdiv = (a, b) => rat(a.n * b.d, a.d * b.n);
const rneg = (a) => ({ n: -a.n, d: a.d });
const rsign = (a) => (a.n > 0n ? 1 : a.n < 0n ? -1 : 0);
const rcmp = (a, b) => rsign(rsub(a, b));
const rabs = (a) => (a.n < 0n ? rneg(a) : a);

const isqrt = (n) => {
  if (n < 0n) return null;
  if (n < 2n) return n;
  let x = BigInt(Math.floor(Math.sqrt(Number(n))));
  while (x * x > n) x -= 1n;
  while ((x + 1n) * (x + 1n) <= n) x += 1n;
  return x * x === n ? x : null;
};

const rsqrt = (a) => {
  if (a.n < 0n) return null;
  const n = isqrt(a.n);
  const d = isqrt(a.d);
  return n === null || d === null ? null : { n, d };
};

// ---------------------------------------------------------------------------
// Monomials

const trim = (e) => {
  let k = e.length;
  while (k > 0 && e[k - 1] === 0) k--;
  return k === e.length ? e : e.slice(0, k);
};
const keyOf = (e) => e.join(",");
const emul = (a, b) => {
  const n = Math.max(a.length, b.length);
  const r = new Array(n);
  for (let i = 0; i < n; i++) r[i] = (a[i] || 0) + (b[i] || 0);
  return r;
};
// a / b, or null when b does not divide a
const ediv = (a, b) => {
  const n = Math.max(a.length, b.length);
  const r = new Array(n);
  for (let i = 0; i < n; i++) {
    const x = (a[i] || 0) - (b[i] || 0);
    if (x < 0) return null;
    r[i] = x;
  }
  return trim(r);
};
// lexicographic, highest variable most significant
const ecmp = (a, b) => {
  const n = Math.max(a.length, b.length);
  for (let i = n - 1; i >= 0; i--) {
    const x = (a[i] || 0) - (b[i] || 0);
    if (x !== 0) return x > 0 ? 1 : -1;
  }
  return 0;
};

// ---------------------------------------------------------------------------
// Construction and arithmetic

const ZERO = new Map();

const addTerm = (m, e, c) => {
  if (c.n === 0n) return;
  const k = keyOf(e);
  const t = m.get(k);
  if (t === undefined) m.set(k, { e, n: c.n, d: c.d });
  else {
    const s = radd(t, c);
    if (s.n === 0n) m.delete(k);
    else m.set(k, { e, n: s.n, d: s.d });
  }
};

const constant = (c) => {
  const m = new Map();
  addTerm(m, [], c);
  return m;
};

const variable = (i) => {
  const e = new Array(i + 1).fill(0);
  e[i] = 1;
  const m = new Map();
  m.set(keyOf(e), { e, n: 1n, d: 1n });
  return m;
};

const monomial = (e, c) => {
  const m = new Map();
  addTerm(m, trim(e), c);
  return m;
};

const add = (a, b) => {
  if (a.size === 0) return b;
  if (b.size === 0) return a;
  const m = new Map(a);
  for (const t of b.values()) addTerm(m, t.e, t);
  return m;
};

const neg = (a) => {
  const m = new Map();
  for (const [k, t] of a) m.set(k, { e: t.e, n: -t.n, d: t.d });
  return m;
};

const sub = (a, b) => add(a, neg(b));

const scale = (c, a) => {
  if (c.n === 0n) return ZERO;
  const m = new Map();
  for (const [k, t] of a) {
    const s = rmul(t, c);
    m.set(k, { e: t.e, n: s.n, d: s.d });
  }
  return m;
};

const mul = (a, b) => {
  if (a.size === 0 || b.size === 0) return ZERO;
  const m = new Map();
  for (const s of a.values()) for (const t of b.values()) addTerm(m, emul(s.e, t.e), rmul(s, t));
  return m;
};

const mulTerm = (a, e, c) => {
  const m = new Map();
  for (const t of a.values()) addTerm(m, emul(t.e, e), rmul(t, c));
  return m;
};

const pow = (a, k) => {
  let r = constant(RONE);
  let b = a;
  while (k > 0) {
    if (k & 1) r = mul(r, b);
    b = mul(b, b);
    k >>= 1;
  }
  return r;
};

const equal = (a, b) => {
  if (a.size !== b.size) return false;
  for (const [k, t] of a) {
    const s = b.get(k);
    if (s === undefined || s.n !== t.n || s.d !== t.d) return false;
  }
  return true;
};

// The leading term (in the monomial order).
const leading = (a) => {
  let best = null;
  for (const t of a.values()) if (best === null || ecmp(t.e, best.e) > 0) best = t;
  return best;
};

const constValue = (a) => {
  if (a.size === 0) return RZERO;
  if (a.size === 1) {
    const t = a.values().next().value;
    if (t.e.length === 0) return { n: t.n, d: t.d };
  }
  return null;
};

const maxVar = (a) => {
  let v = -1;
  for (const t of a.values()) v = Math.max(v, t.e.length - 1);
  return v;
};

const vars = (a) => {
  const s = new Set();
  for (const t of a.values()) t.e.forEach((x, i) => x > 0 && s.add(i));
  return [...s].sort((x, y) => x - y);
};

const degIn = (a, v) => {
  let d = 0;
  for (const t of a.values()) d = Math.max(d, t.e[v] || 0);
  return d;
};

const totalDeg = (a) => {
  let d = 0;
  for (const t of a.values()) d = Math.max(d, t.e.reduce((s, x) => s + x, 0));
  return d;
};

// Coefficients of a as a polynomial in v: result[k] is the coefficient of v^k.
const coeffsIn = (a, v) => {
  const out = [];
  for (const t of a.values()) {
    const k = t.e[v] || 0;
    const e = t.e.slice();
    if (v < e.length) e[v] = 0;
    while (out.length <= k) out.push(new Map());
    addTerm(out[k], trim(e), t);
  }
  return out;
};

const fromCoeffsIn = (cs, v) => {
  let r = ZERO;
  cs.forEach((c, k) => {
    if (c.size === 0) return;
    const e = new Array(v + 1).fill(0);
    e[v] = k;
    r = add(r, mulTerm(c, trim(e), RONE));
  });
  return r;
};

// ---------------------------------------------------------------------------
// Evaluation and substitution

const powR = (r, k) => {
  let out = RONE;
  for (let i = 0; i < k; i++) out = rmul(out, r);
  return out;
};

// vals: array (index = variable) of rationals; missing variables stay.
const substRats = (a, vals) => {
  const m = new Map();
  for (const t of a.values()) {
    let c = { n: t.n, d: t.d };
    const e = t.e.slice();
    for (let i = 0; i < e.length; i++) {
      if (e[i] > 0 && vals[i] !== undefined && vals[i] !== null) {
        c = rmul(c, powR(vals[i], e[i]));
        e[i] = 0;
      }
    }
    addTerm(m, trim(e), c);
  }
  return m;
};

// D^k · a(v := N/D), with k = deg_v a.
const substFrac = (a, v, num, den) => {
  const cs = coeffsIn(a, v);
  const k = cs.length - 1;
  let r = ZERO;
  cs.forEach((c, i) => {
    if (c.size === 0) return;
    r = add(r, mul(c, mul(pow(num, i), pow(den, k - i))));
  });
  return { poly: r, degree: Math.max(k, 0) };
};

const deriv = (a, v) => {
  const m = new Map();
  for (const t of a.values()) {
    const k = t.e[v] || 0;
    if (k === 0) continue;
    const e = t.e.slice();
    e[v] = k - 1;
    addTerm(m, trim(e), rmul(t, { n: BigInt(k), d: 1n }));
  }
  return m;
};

// ---------------------------------------------------------------------------
// Division and gcd

// a / b when the division is exact, else null.
const divExact = (a, b) => {
  if (b.size === 0) throw new RangeError("Poly: division by zero");
  const lb = leading(b);
  let q = new Map();
  let r = a;
  let guard = 0;
  while (r.size > 0) {
    if (++guard > 100000) return null;
    const lr = leading(r);
    const e = ediv(lr.e, lb.e);
    if (e === null) return null;
    const c = rdiv(lr, lb);
    addTerm(q, e, c);
    r = sub(r, mulTerm(b, e, c));
  }
  return q;
};

// The integer content and sign: a = c · p with p integral, primitive, and a
// positive leading coefficient.
const normalise = (a) => {
  if (a.size === 0) return { c: RZERO, p: ZERO };
  let den = 1n;
  for (const t of a.values()) den = (den / igcd(den, t.d)) * t.d;
  let g = 0n;
  for (const t of a.values()) g = igcd(g, (t.n * den) / t.d);
  const lt = leading(a);
  const sign = lt.n < 0n ? -1n : 1n;
  const c = rat(g * sign, den);
  return { c, p: scale(rdiv(RONE, c), a) };
};

const primitive = (a) => normalise(a).p;

// Pseudo-remainder of a by b in the variable v.
const prem = (a, b, v) => {
  const db = degIn(b, v);
  const lcb = coeffsIn(b, v)[db];
  let r = a;
  let guard = 0;
  while (r.size > 0) {
    const dr = degIn(r, v);
    if (dr < db) break;
    if (++guard > 10000) throw new Error("Poly.prem: no progress");
    const lcr = coeffsIn(r, v)[dr];
    const e = new Array(v + 1).fill(0);
    e[v] = dr - db;
    r = sub(mul(lcb, r), mulTerm(mul(lcr, b), trim(e), RONE));
  }
  return r;
};

// The gcd as a primitive integral polynomial with a positive leading
// coefficient (over Q it is defined up to a constant factor).
const gcd = (a, b) => {
  if (a.size === 0) return b.size === 0 ? ZERO : primitive(b);
  if (b.size === 0) return primitive(a);
  if (constValue(a) !== null || constValue(b) !== null) return constant(RONE);
  const v = Math.max(maxVar(a), maxVar(b));
  const da = degIn(a, v);
  const db = degIn(b, v);
  if (da === 0) return gcd(a, contentIn(b, v));
  if (db === 0) return gcd(contentIn(a, v), b);
  const ca = contentIn(a, v);
  const cb = contentIn(b, v);
  let p = primitive(divExact(a, ca));
  let q = primitive(divExact(b, cb));
  if (degIn(p, v) < degIn(q, v)) [p, q] = [q, p];
  while (q.size > 0) {
    const r = prem(p, q, v);
    p = q;
    q = r.size === 0 ? r : primitive(ppIn(r, v));
    if (q.size > 0 && degIn(q, v) === 0) {
      // the gcd has degree 0 in v
      p = constant(RONE);
      q = ZERO;
    }
  }
  const g = degIn(p, v) === 0 ? constant(RONE) : primitive(ppIn(p, v));
  return primitive(mul(gcd(ca, cb), g));
};

// The content of a in v: the gcd of its coefficients as a polynomial in v.
const contentIn = (a, v) => {
  const cs = coeffsIn(a, v).filter((c) => c.size > 0);
  let g = ZERO;
  for (const c of cs) {
    g = gcd(g, c);
    if (constValue(g) !== null) return constant(RONE);
  }
  return g;
};

const ppIn = (a, v) => divExact(a, contentIn(a, v));

// ---------------------------------------------------------------------------
// Square roots

// s with s² = a and a positive leading coefficient, or null.
const sqrt = (a) => {
  if (a.size === 0) return ZERO;
  const lt = leading(a);
  if (lt.e.some((x) => x % 2 !== 0)) return null;
  const c0 = rsqrt({ n: lt.n, d: lt.d });
  if (c0 === null) return null;
  const s0 = { e: lt.e.map((x) => x / 2), c: c0 };
  let s = monomial(s0.e, c0);
  let r = sub(a, mul(s, s));
  let guard = a.size + 2;
  while (r.size > 0) {
    if (--guard < 0) return null;
    const lr = leading(r);
    const e = ediv(lr.e, s0.e);
    if (e === null || ecmp(e, s0.e) >= 0) return null;
    const c = rdiv(lr, rmul({ n: 2n, d: 1n }, c0));
    s = add(s, monomial(e, c));
    r = sub(a, mul(s, s));
  }
  return s;
};

// ---------------------------------------------------------------------------
// Quadratic forms: the sign of a polynomial of total degree ≤ 2 on all of ℝⁿ

const det = (m) => {
  // exact Gaussian elimination
  const n = m.length;
  const a = m.map((row) => row.slice());
  let d = RONE;
  for (let i = 0; i < n; i++) {
    let p = i;
    while (p < n && a[p][i].n === 0n) p++;
    if (p === n) return RZERO;
    if (p !== i) {
      [a[p], a[i]] = [a[i], a[p]];
      d = rneg(d);
    }
    d = rmul(d, a[i][i]);
    for (let r = i + 1; r < n; r++) {
      if (a[r][i].n === 0n) continue;
      const f = rdiv(a[r][i], a[i][i]);
      for (let c = i; c < n; c++) a[r][c] = rsub(a[r][c], rmul(f, a[i][c]));
    }
  }
  return d;
};

// "pd" (> 0 everywhere), "psd" (≥ 0), "nd", "nsd", or "indef"; null when the
// total degree exceeds 2.
const quadSign = (a) => {
  if (totalDeg(a) > 2) return null;
  const vs = vars(a);
  const n = vs.length + 1;
  const m = [];
  for (let i = 0; i < n; i++) m.push(new Array(n).fill(RZERO));
  const half = { n: 1n, d: 2n };
  for (const t of a.values()) {
    const idx = [];
    t.e.forEach((x, v) => {
      for (let k = 0; k < x; k++) idx.push(vs.indexOf(v) + 1);
    });
    const c = { n: t.n, d: t.d };
    if (idx.length === 0) m[0][0] = radd(m[0][0], c);
    else if (idx.length === 1) {
      m[0][idx[0]] = radd(m[0][idx[0]], rmul(c, half));
      m[idx[0]][0] = radd(m[idx[0]][0], rmul(c, half));
    } else if (idx[0] === idx[1]) m[idx[0]][idx[0]] = radd(m[idx[0]][idx[0]], c);
    else {
      m[idx[0]][idx[1]] = radd(m[idx[0]][idx[1]], rmul(c, half));
      m[idx[1]][idx[0]] = radd(m[idx[1]][idx[0]], rmul(c, half));
    }
  }
  const minor = (rows) => det(rows.map((i) => rows.map((j) => m[i][j])));
  const subsets = [];
  for (let mask = 1; mask < 1 << n; mask++) {
    const s = [];
    for (let i = 0; i < n; i++) if (mask & (1 << i)) s.push(i);
    subsets.push(s);
  }
  const lead = (k) => minor([...Array(k).keys()]);
  const test = (sgn) => {
    // sgn = 1 for positive, -1 for negative (minors of −M)
    let pd = true;
    for (let k = 1; k <= n; k++) {
      const d = lead(k);
      const want = sgn === 1 || k % 2 === 0 ? 1 : -1;
      if (rsign(d) !== want) pd = false;
    }
    if (pd) return "d";
    for (const s of subsets) {
      const d = minor(s);
      const want = sgn === 1 || s.length % 2 === 0 ? 1 : -1;
      if (rsign(d) !== 0 && rsign(d) !== want) return null;
    }
    return "sd";
  };
  const pos = test(1);
  if (pos === "d") return "pd";
  const neg = test(-1);
  if (neg === "d") return "nd";
  if (pos === "sd") return "psd";
  if (neg === "sd") return "nsd";
  return "indef";
};

// The minimiser of a quadratic whose quadratic part is nonsingular: the
// unique x* with ∇f(x*) = 0, with f(x*). For a positive semidefinite f this
// is its only possible zero. { vars, point, value } or null.
const quadMin = (a) => {
  if (totalDeg(a) > 2) return null;
  const vs = vars(a);
  const n = vs.length;
  if (n === 0) return null;
  // ∇f = 0: rows i: Σ_j ∂²f/∂xi∂xj xj = −∂f/∂xi(0)
  const m = [];
  for (let i = 0; i < n; i++) m.push(new Array(n + 1).fill(RZERO));
  for (const t of a.values()) {
    const idx = [];
    t.e.forEach((x, v) => {
      for (let k = 0; k < x; k++) idx.push(vs.indexOf(v));
    });
    const c = { n: t.n, d: t.d };
    if (idx.length === 1) m[idx[0]][n] = rsub(m[idx[0]][n], c);
    else if (idx.length === 2) {
      if (idx[0] === idx[1]) m[idx[0]][idx[0]] = radd(m[idx[0]][idx[0]], rmul(c, { n: 2n, d: 1n }));
      else {
        m[idx[0]][idx[1]] = radd(m[idx[0]][idx[1]], c);
        m[idx[1]][idx[0]] = radd(m[idx[1]][idx[0]], c);
      }
    }
  }
  // Gauss–Jordan
  for (let i = 0; i < n; i++) {
    let p = i;
    while (p < n && m[p][i].n === 0n) p++;
    if (p === n) return null;
    [m[p], m[i]] = [m[i], m[p]];
    const piv = m[i][i];
    for (let c = i; c <= n; c++) m[i][c] = rdiv(m[i][c], piv);
    for (let r = 0; r < n; r++) {
      if (r === i || m[r][i].n === 0n) continue;
      const f = m[r][i];
      for (let c = i; c <= n; c++) m[r][c] = rsub(m[r][c], rmul(f, m[i][c]));
    }
  }
  const point = m.map((row) => row[n]);
  const vals = [];
  vs.forEach((v, i) => (vals[v] = point[i]));
  const value = constValue(substRats(a, vals));
  return { vars: vs, point, value };
};

// A positive semidefinite quadratic as a sum of squares: f = Σ dᵢ ℓᵢ² with
// dᵢ ≥ 0 rational and ℓᵢ linear forms (LDLᵀ of the matrix of the form in
// [1, x₁, …]). Null when f is not a quadratic or not semidefinite.
const ldl = (a, ord) => {
  if (totalDeg(a) > 2) return null;
  const own = vars(a);
  const vs = ord === undefined ? own : ord;
  if (own.some((v) => !vs.includes(v))) return null;
  const n = vs.length + 1;
  const m = [];
  for (let i = 0; i < n; i++) m.push(new Array(n).fill(RZERO));
  const half = { n: 1n, d: 2n };
  for (const t of a.values()) {
    const idx = [];
    t.e.forEach((x, v) => {
      for (let k = 0; k < x; k++) idx.push(vs.indexOf(v) + 1);
    });
    const c = { n: t.n, d: t.d };
    if (idx.length === 0) m[0][0] = radd(m[0][0], c);
    else if (idx.length === 1) {
      m[0][idx[0]] = radd(m[0][idx[0]], rmul(c, half));
      m[idx[0]][0] = radd(m[idx[0]][0], rmul(c, half));
    } else if (idx[0] === idx[1]) m[idx[0]][idx[0]] = radd(m[idx[0]][idx[0]], c);
    else {
      m[idx[0]][idx[1]] = radd(m[idx[0]][idx[1]], rmul(c, half));
      m[idx[1]][idx[0]] = radd(m[idx[1]][idx[0]], rmul(c, half));
    }
  }
  // the variables first, the constant last, so that a sum of squares of
  // linear forms comes out as such
  const order = [...Array(n).keys()].slice(1).concat([0]);
  const L = [];
  for (let i = 0; i < n; i++) L.push(new Array(n).fill(RZERO));
  const d = new Array(n).fill(RZERO);
  for (let i = 0; i < n; i++) {
    const oi = order[i];
    let di = m[oi][oi];
    for (let k = 0; k < i; k++) di = rsub(di, rmul(rmul(L[i][k], L[i][k]), d[k]));
    if (rsign(di) < 0) return null;
    d[i] = di;
    L[i][i] = RONE;
    for (let j = i + 1; j < n; j++) {
      const oj = order[j];
      let v = m[oj][oi];
      for (let k = 0; k < i; k++) v = rsub(v, rmul(rmul(L[j][k], L[i][k]), d[k]));
      if (di.n === 0n) {
        if (v.n !== 0n) return null;
        L[j][i] = RZERO;
      } else L[j][i] = rdiv(v, di);
    }
  }
  // ℓᵢ = Σⱼ L[j][i] zⱼ with z the variables in `order`
  const out = [];
  for (let i = 0; i < n; i++) {
    if (d[i].n === 0n) continue;
    let l = ZERO;
    for (let j = i; j < n; j++) {
      if (L[j][i].n === 0n) continue;
      const oj = order[j];
      l = add(l, oj === 0 ? constant(L[j][i]) : scale(L[j][i], variable(vs[oj - 1])));
    }
    out.push({ d: d[i], l });
  }
  return out;
};

// ---------------------------------------------------------------------------
// Univariate polynomials: arrays of rationals, lowest degree first

const toUni = (a, v) => {
  const out = [];
  for (const t of a.values()) {
    const k = t.e[v] || 0;
    if (t.e.some((x, i) => i !== v && x > 0)) return null;
    while (out.length <= k) out.push(RZERO);
    out[k] = radd(out[k], t);
  }
  return utrim(out);
};

const utrim = (p) => {
  let k = p.length;
  while (k > 0 && p[k - 1].n === 0n) k--;
  return p.slice(0, k);
};
const udeg = (p) => p.length - 1;
const ueval = (p, x) => {
  let r = RZERO;
  for (let i = p.length - 1; i >= 0; i--) r = radd(rmul(r, x), p[i]);
  return r;
};
const uderiv = (p) => utrim(p.slice(1).map((c, i) => rmul(c, { n: BigInt(i + 1), d: 1n })));
const urem = (a, b) => {
  let r = a.slice();
  const db = udeg(b);
  const lb = b[db];
  while (r.length > 0 && udeg(r) >= db) {
    const k = udeg(r) - db;
    const f = rdiv(r[udeg(r)], lb);
    for (let i = 0; i <= db; i++) r[i + k] = rsub(r[i + k], rmul(f, b[i]));
    r = utrim(r);
  }
  return r;
};
const udiv = (a, b) => {
  let r = a.slice();
  const db = udeg(b);
  const lb = b[db];
  const q = new Array(Math.max(0, udeg(a) - db + 1)).fill(RZERO);
  while (r.length > 0 && udeg(r) >= db) {
    const k = udeg(r) - db;
    const f = rdiv(r[udeg(r)], lb);
    q[k] = f;
    for (let i = 0; i <= db; i++) r[i + k] = rsub(r[i + k], rmul(f, b[i]));
    r = utrim(r);
  }
  return utrim(q);
};
const ugcd = (a, b) => {
  while (b.length > 0) [a, b] = [b, urem(a, b)];
  if (a.length === 0) return a;
  const lc = a[udeg(a)];
  return a.map((c) => rdiv(c, lc));
};
const usqfree = (p) => (udeg(p) < 1 ? p : udiv(p, ugcd(p, uderiv(p))));

const sturm = (p) => {
  const seq = [p, uderiv(p)];
  while (seq[seq.length - 1].length > 0) {
    const r = urem(seq[seq.length - 2], seq[seq.length - 1]);
    if (r.length === 0) break;
    seq.push(r.map(rneg));
  }
  return seq.filter((q) => q.length > 0);
};
const changes = (signs) => {
  let c = 0;
  let last = 0;
  for (const s of signs) {
    if (s === 0) continue;
    if (last !== 0 && s !== last) c++;
    last = s;
  }
  return c;
};
const signsAt = (seq, x) => seq.map((q) => rsign(ueval(q, x)));
// distinct real roots of p in (a, b], from its Sturm sequence
const countRoots = (seq, a, b) => changes(signsAt(seq, a)) - changes(signsAt(seq, b));

const cauchy = (p) => {
  const n = udeg(p);
  let m = RZERO;
  for (let i = 0; i < n; i++) {
    const q = rabs(rdiv(p[i], p[n]));
    if (rcmp(q, m) > 0) m = q;
  }
  return radd(m, RONE);
};

// Isolating intervals for the distinct real roots of p, in increasing order:
// { lo, hi } with lo < root < hi and p(lo), p(hi) ≠ 0, or lo = hi = an exact
// rational root.
const isolate = (p0) => {
  const p = usqfree(utrim(p0));
  if (udeg(p) < 1) return [];
  const seq = sturm(p);
  const b = cauchy(p);
  const out = [];
  const half = { n: 1n, d: 2n };
  const isRoot = (x) => ueval(p, x).n === 0n;
  // lo and hi are never roots
  const go = (lo, hi, depth) => {
    const k = countRoots(seq, lo, hi);
    if (k === 0) return;
    if (k === 1) {
      out.push({ lo, hi });
      return;
    }
    if (depth > 2000) throw new Error("Poly.isolate: no separation");
    const mid = rmul(radd(lo, hi), half);
    if (!isRoot(mid)) {
      go(lo, mid, depth + 1);
      go(mid, hi, depth + 1);
      return;
    }
    let d = rmul(rsub(hi, lo), { n: 1n, d: 4n });
    while (isRoot(rsub(mid, d)) || isRoot(radd(mid, d)) || countRoots(seq, rsub(mid, d), radd(mid, d)) > 1)
      d = rmul(d, half);
    out.push({ lo: mid, hi: mid });
    go(lo, rsub(mid, d), depth + 1);
    go(radd(mid, d), hi, depth + 1);
  };
  go(rneg(b), b, 0);
  out.sort((x, y) => rcmp(x.lo, y.lo));
  return out;
};

const refine = (p, iv, width) => {
  let { lo, hi } = iv;
  if (rcmp(lo, hi) === 0) return iv;
  const half = { n: 1n, d: 2n };
  const slo = rsign(ueval(p, lo));
  while (rcmp(rsub(hi, lo), width) > 0) {
    const mid = rmul(radd(lo, hi), half);
    const sm = rsign(ueval(p, mid));
    if (sm === 0) return { lo: mid, hi: mid };
    if (sm === slo) lo = mid;
    else hi = mid;
  }
  return { lo, hi };
};

// a rational strictly between x and y (x < y), with a small denominator
const between = (x, y) => {
  for (let den = 1n; den < 1n << 40n; den *= 2n) {
    // the smallest multiple of 1/den above x
    let n = (x.n * den) / x.d;
    if (rcmp(rat(n, den), x) <= 0) n += 1n;
    const c = rat(n, den);
    if (rcmp(c, y) < 0) return c;
  }
  return rmul(radd(x, y), { n: 1n, d: 2n });
};

// Cells of ℝ for the univariate polynomials ps: their common real roots and
// the open intervals between them, each with a sample and the signs of every
// p in ps. A root cell has `exact` when the root is rational.
const cells = (ps) => {
  const nonzero = ps.filter((p) => p.length > 0);
  let prod = [RONE];
  for (const p of nonzero) prod = umul(prod, usqfree(p));
  const sf = usqfree(prod);
  const rational = ratRoots(sf) || [];
  const roots = isolate(prod).map((iv) => {
    const r = rational.find((x) => rcmp(iv.lo, iv.hi) !== 0 && rcmp(x, iv.lo) > 0 && rcmp(x, iv.hi) < 0);
    return r !== undefined ? { lo: r, hi: r } : refine(sf, iv, { n: 1n, d: 1n << 20n });
  });
  const out = [];
  const signsOpen = (x) => ps.map((p) => rsign(ueval(p, x)));
  const signsRoot = (iv) => {
    if (rcmp(iv.lo, iv.hi) === 0) return ps.map((p) => rsign(ueval(p, iv.lo)));
    return ps.map((p) => {
      if (p.length === 0) return 0;
      const g = ugcd(p, sf);
      if (udeg(g) >= 1 && countRoots(sturm(g), iv.lo, iv.hi) > 0) return 0;
      return rsign(ueval(p, iv.lo));
    });
  };
  if (roots.length === 0) {
    out.push({ kind: "open", sample: RZERO, signs: signsOpen(RZERO) });
    return out;
  }
  const first = rsub(roots[0].lo, RONE);
  out.push({ kind: "open", sample: first, signs: signsOpen(first) });
  roots.forEach((iv, i) => {
    out.push({
      kind: "root",
      sample: rcmp(iv.lo, iv.hi) === 0 ? iv.lo : null,
      lo: iv.lo,
      hi: iv.hi,
      signs: signsRoot(iv),
    });
    const next = roots[i + 1];
    const x = next ? between(iv.hi, next.lo) : radd(iv.hi, RONE);
    out.push({ kind: "open", sample: x, signs: signsOpen(x) });
  });
  return out;
};

const umul = (a, b) => {
  if (a.length === 0 || b.length === 0) return [];
  const r = new Array(a.length + b.length - 1).fill(RZERO);
  for (let i = 0; i < a.length; i++) for (let j = 0; j < b.length; j++) r[i + j] = radd(r[i + j], rmul(a[i], b[j]));
  return utrim(r);
};

// The rational roots of a univariate polynomial.
const ratRoots = (p0) => {
  const p = usqfree(utrim(p0));
  if (udeg(p) < 1) return [];
  // integral coefficients
  let den = 1n;
  for (const c of p) den = (den / igcd(den, c.d)) * c.d;
  const ints = p.map((c) => (c.n * den) / c.d);
  const lead = ints[ints.length - 1] < 0n ? -ints[ints.length - 1] : ints[ints.length - 1];
  if (lead > 10n ** 12n) return null;
  const divisors = [];
  for (let k = 1n; k * k <= lead; k++)
    if (lead % k === 0n) {
      divisors.push(k);
      if (k * k !== lead) divisors.push(lead / k);
    }
  const out = [];
  for (const iv0 of isolate(p)) {
    if (rcmp(iv0.lo, iv0.hi) === 0) {
      out.push(iv0.lo);
      continue;
    }
    const maxDen = divisors.reduce((m, x) => (x > m ? x : m), 1n);
    const iv = refine(p, iv0, rat(1n, 2n * maxDen * maxDen + 1n));
    if (rcmp(iv.lo, iv.hi) === 0) {
      out.push(iv.lo);
      continue;
    }
    for (const q of divisors) {
      // integers n with lo < n/q < hi
      let n = (iv.lo.n * q) / iv.lo.d;
      for (let k = 0; k < 3; k++, n += 1n) {
        const c = rat(n, q);
        if (rcmp(c, iv.lo) > 0 && rcmp(c, iv.hi) < 0 && ueval(p, c).n === 0n) {
          out.push(c);
          k = 3;
        }
      }
    }
  }
  return out;
};

// ---------------------------------------------------------------------------
// Exports (curried for PureScript)

const toR = (r) => ({ n: r.n, d: r.d });

export const zeroP = ZERO;
export const nullR = null;
export const someR = (r) => r;
export const constP = (c) => constant(toR(c));
export const varP = (i) => variable(i);
export const addP = (a) => (b) => add(a, b);
export const subP = (a) => (b) => sub(a, b);
export const negP = neg;
export const mulP = (a) => (b) => mul(a, b);
export const scaleP = (c) => (a) => scale(toR(c), a);
export const powP = (a) => (k) => pow(a, k);
export const eqP = (a) => (b) => equal(a, b);
export const isZeroP = (a) => a.size === 0;
export const constValueImpl = (a) => constValue(a);
export const varsP = vars;
export const degInP = (a) => (v) => degIn(a, v);
export const totalDegP = totalDeg;
export const coeffsInP = (a) => (v) => coeffsIn(a, v);
export const fromCoeffsInP = (cs) => (v) => fromCoeffsIn(cs, v);
export const termsP = (a) =>
  [...a.values()].sort((s, t) => -ecmp(s.e, t.e)).map((t) => ({ exps: t.e, coeff: { n: t.n, d: t.d } }));
export const leadingCoeffP = (a) => {
  const t = leading(a);
  return t === null ? RZERO : { n: t.n, d: t.d };
};
export const sizeP = (a) => a.size;
// vals: array of nullable rationals indexed by variable
export const substRatsP = (a) => (vals) => substRats(a, vals);
export const substFracP = (a) => (v) => (num) => (den) => substFrac(a, v, num, den);
export const derivP = (a) => (v) => deriv(a, v);
export const divExactImpl = (a) => (b) => divExact(a, b);
export const gcdP = (a) => (b) => gcd(a, b);
export const normaliseP = (a) => normalise(a);
export const sqrtImpl = (a) => sqrt(a);
export const quadSignImpl = (a) => quadSign(a);
export const quadMinImpl = (a) => quadMin(a);
export const ldlImpl = (a) => ldl(a);
export const ldlOrderImpl = (a) => (ord) => ldl(a, ord);
// renames the variables: variable i becomes ren[i]
export const renameP = (a) => (ren) => {
  const m = new Map();
  for (const t of a.values()) {
    const e = [];
    t.e.forEach((x, v) => {
      if (x === 0) return;
      const w = ren[v];
      if (w === undefined || w < 0) throw new RangeError("Poly.rename: variable " + v + " has no new name");
      while (e.length <= w) e.push(0);
      e[w] += x;
    });
    addTerm(m, trim(e), { n: t.n, d: t.d });
  }
  return m;
};
export const keyP = (a) =>
  [...a.values()]
    .sort((s, t) => ecmp(s.e, t.e))
    .map((t) => `${keyOf(t.e)}:${t.n}/${t.d}`)
    .join(";");

// univariate
export const toUniImpl = (a) => (v) => toUni(a, v);
export const cellsP = (ps) => cells(ps);
export const ratRootsImpl = (p) => ratRoots(p);
export const isolateP = (p) => isolate(p);
