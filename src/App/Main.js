// Keeps letters, digits, spaces and hyphens (for file names from titles).
export const onlyWordChars = (s) => s.normalize("NFKD").replace(/[^\p{L}\p{N} -]/gu, " ");
