// Numbers for SVG attributes: up to 5 decimals, no exponent notation.
export const fmt = (x) => {
  if (!Number.isFinite(x)) return "0";
  const s = x.toFixed(5);
  return s.includes(".") ? s.replace(/\.?0+$/, "") : s;
};

export const escapeXml = (s) =>
  s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
