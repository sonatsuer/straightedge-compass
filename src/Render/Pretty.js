export const toPrecision = (digits) => (x) => {
  if (x === 0) return "0";
  const s = x.toPrecision(digits);
  // Drop trailing zeros of the fraction ("0.5000000000" -> "0.5").
  if (s.includes("e")) {
    const [mant, exp] = s.split("e");
    return (mant.includes(".") ? mant.replace(/\.?0+$/, "") : mant) + "e" + exp;
  }
  return s.includes(".") ? s.replace(/\.?0+$/, "") : s;
};
