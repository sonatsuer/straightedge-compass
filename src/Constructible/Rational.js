// Exact rationals over native BigInt.
// Invariant: { n, d } with d > 0n and gcd(|n|, d) === 1n.

const gcd = (a, b) => {
  if (a < 0n) a = -a;
  while (b !== 0n) {
    const t = a % b;
    a = b;
    b = t;
  }
  return a;
};

const mk = (n, d) => {
  if (d === 0n) throw new RangeError("Constructible.Rational: division by zero");
  if (d < 0n) {
    n = -n;
    d = -d;
  }
  if (d === 1n) return { n, d };
  const g = gcd(n, d);
  return g === 1n ? { n, d } : { n: n / g, d: d / g };
};

const ZERO = { n: 0n, d: 1n };
const ONE = { n: 1n, d: 1n };

export const zeroR = ZERO;
export const oneR = ONE;

export const fromInt = (i) => ({ n: BigInt(i), d: 1n });

export const fromIntsImpl = (n) => (d) => mk(BigInt(n), BigInt(d));

// Parses "p", "-p" or "p/q" (decimal integers); returns null on failure.
export const parseImpl = (s) => {
  const m = /^\s*(-?\d+)(?:\s*\/\s*(\d+))?\s*$/.exec(s);
  if (!m) return null;
  const d = m[2] === undefined ? 1n : BigInt(m[2]);
  if (d === 0n) return null;
  return mk(BigInt(m[1]), d);
};

export const addR = (a) => (b) =>
  a.d === b.d ? mk(a.n + b.n, a.d) : mk(a.n * b.d + b.n * a.d, a.d * b.d);

export const subR = (a) => (b) =>
  a.d === b.d ? mk(a.n - b.n, a.d) : mk(a.n * b.d - b.n * a.d, a.d * b.d);

export const mulR = (a) => (b) => {
  if (a.n === 0n || b.n === 0n) return ZERO;
  return mk(a.n * b.n, a.d * b.d);
};

export const negateR = (a) => ({ n: -a.n, d: a.d });

export const recipR = (a) => mk(a.d, a.n);

export const divR = (a) => (b) => mk(a.n * b.d, a.d * b.n);

export const eqR = (a) => (b) => a.n === b.n && a.d === b.d;

// -1, 0 or 1
export const compareImpl = (a) => (b) => {
  const l = a.d === b.d ? a.n : a.n * b.d;
  const r = a.d === b.d ? b.n : b.n * a.d;
  return l < r ? -1 : l > r ? 1 : 0;
};

export const signum = (a) => (a.n < 0n ? -1 : a.n > 0n ? 1 : 0);

export const isZero = (a) => a.n === 0n;

export const isInteger = (a) => a.d === 1n;

// Integer square root (floor) of a non-negative BigInt, by Newton's method.
const isqrt = (n) => {
  if (n < 2n) return n;
  let x = 1n << BigInt(Math.ceil(n.toString(2).length / 2));
  for (;;) {
    const y = (x + n / x) >> 1n;
    if (y >= x) return x;
    x = y;
  }
};

// The rational square root if it exists (both n and d perfect squares).
export const exactSqrtImpl = (a) => {
  if (a.n < 0n) return null;
  const rn = isqrt(a.n);
  if (rn * rn !== a.n) return null;
  const rd = isqrt(a.d);
  if (rd * rd !== a.d) return null;
  return { n: rn, d: rd };
};

// Largest integer <= a.
export const floor = (a) => {
  let q = a.n / a.d; // truncates toward zero
  if (a.n < 0n && q * a.d !== a.n) q -= 1n;
  return { n: q, d: 1n };
};

const bitLength = (x) => (x === 0n ? 0 : x.toString(2).length);

// Correctly rounded conversion to a double (in the normal range).
export const toNumber = (a) => {
  if (a.n === 0n) return 0;
  const neg = a.n < 0n;
  const n = neg ? -a.n : a.n;
  // Choose a shift so that the integer quotient has at least 64 bits.
  let shift = bitLength(a.d) - bitLength(n) + 64;
  let num = n;
  let den = a.d;
  if (shift >= 0) num <<= BigInt(shift);
  else den <<= BigInt(-shift);
  let q = num / den;
  if (q * den !== num) {
    // Sticky bit: makes Number(q) round exactly as the true quotient would.
    q = (q << 1n) | 1n;
    shift += 1;
  }
  // Split the scaling so that 2^-shift does not underflow prematurely.
  const half = Math.trunc(shift / 2);
  const x = Number(q) * Math.pow(2, -half) * Math.pow(2, -(shift - half));
  return neg ? -x : x;
};

export const numerator = (a) => ({ n: a.n, d: 1n });
export const denominator = (a) => ({ n: a.d, d: 1n });

// Haskell-style rendering of the numerator and the denominator.
export const numeratorString = (a) => a.n.toString();
export const denominatorString = (a) => a.d.toString();

export const toString = (a) => (a.d === 1n ? a.n.toString() : `${a.n}/${a.d}`);

// Integer power with an Int exponent (negative exponents allowed).
export const powImpl = (a) => (k) => {
  if (k >= 0) return { n: a.n ** BigInt(k), d: a.d ** BigInt(k) };
  return mk(a.d ** BigInt(-k), a.n ** BigInt(-k));
};

// Exact JSON-friendly view: decimal strings of numerator and denominator.
export const toPair = (a) => ({ num: a.n.toString(), den: a.d.toString() });

// n = out^2 * rest for a positive integer rational n, trying primes < 10^4.
export const extractSquares = (q) => {
  let n = q.n < 0n ? -q.n : q.n;
  let out = 1n;
  for (let p = 2n; p < 10000n && p * p <= n; p += p === 2n ? 1n : 2n) {
    const pp = p * p;
    while (n % pp === 0n) {
      n /= pp;
      out *= p;
    }
  }
  return { out: { n: out, d: 1n }, rest: { n, d: 1n } };
};

// gcd of two integer-valued rationals (non-negative result).
export const gcdInt = (a) => (b) => ({ n: gcd(a.n, b.n < 0n ? -b.n : b.n), d: 1n });
