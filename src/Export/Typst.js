// Escapes a string for a Typst string literal.
export const escape = (s) =>
  s.replace(/\\/g, "\\\\").replace(/"/g, '\\"').replace(/\n/g, "\\n").replace(/\r/g, "");
