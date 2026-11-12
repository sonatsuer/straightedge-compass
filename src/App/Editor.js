import { EditorState, RangeSetBuilder, StateEffect, StateField } from "@codemirror/state";
import {
  Decoration,
  EditorView,
  drawSelection,
  highlightActiveLine,
  keymap,
  lineNumbers,
} from "@codemirror/view";
import { defaultKeymap, history, historyKeymap, indentWithTab } from "@codemirror/commands";
import {
  HighlightStyle,
  StreamLanguage,
  StringStream,
  syntaxHighlighting,
} from "@codemirror/language";
import { tags } from "@lezer/highlight";

const KEYWORDS = new Set([
  "given", "line", "circle", "meet", "except", "nearest", "farthest", "left", "right",
  "assert", "note", "title", "def", "end", "on", "dist", "collinear", "concyclic",
  "parallel", "perpendicular", "angle", "ccw", "between", "snapshot",
  "param", "assume", "sample", "cases", "point", "side", "compare",
  "show", "segment", "polygon", "through",
  // branch labels of cases
  "none", "one", "two", "same", "less", "equal", "greater", "different", "yes", "no",
]);

// One token of a script line; the kinds are those of the highlight style.
const token = (stream) => {
    if (stream.eatSpace()) return null;
    if (stream.match("--")) {
      stream.skipToEnd();
      return "comment";
    }
    if (stream.peek() === '"') {
      stream.next();
      while (!stream.eol()) {
        const c = stream.next();
        if (c === "\\") stream.next();
        else if (c === '"') break;
      }
      return "string";
    }
    if (stream.match(/^-?\d+(\/\d+)?/)) return "number";
    if (stream.match(/^[\p{L}_][\p{L}\p{N}_']*/u)) {
      const word = stream.current();
      if (KEYWORDS.has(word)) return "keyword";
      return stream.peek() === "(" ? "macroName" : "variableName";
    }
    if (stream.match(/^(==|!=|<=|>=|->|[<>=,():~])/)) return "operator";
    stream.next();
    return null;
};

const scriptLanguage = StreamLanguage.define({
  name: "construction",
  token,
  tokenTable: { macroName: tags.function(tags.variableName) },
});

// The tokens of a snippet with their kinds, for highlighting outside the
// editor (the Help dialog) with the same rules.
export const tokensOf = (text) => {
  const out = [];
  text.split("\n").forEach((line, i) => {
    if (i > 0) out.push({ text: "\n", kind: "" });
    const stream = new StringStream(line, 2, 2);
    while (!stream.eol()) {
      const start = stream.pos;
      // what CodeMirror does before each token: `current()` is this token
      stream.start = start;
      const kind = token(stream) || "";
      if (stream.pos === start) stream.next();
      out.push({ text: line.slice(start, stream.pos), kind });
    }
  });
  return out;
};

// Colours come from CSS variables, so the editor follows the page theme.
const highlight = HighlightStyle.define([
  { tag: tags.keyword, color: "var(--code-keyword)", fontWeight: "600" },
  { tag: tags.comment, color: "var(--code-comment)", fontStyle: "italic" },
  { tag: tags.string, color: "var(--code-string)" },
  { tag: tags.number, color: "var(--code-number)" },
  { tag: tags.function(tags.variableName), color: "var(--code-macro)" },
  { tag: tags.operator, color: "var(--muted)" },
]);

const setMarks = StateEffect.define();

const marks = StateField.define({
  create: () => Decoration.none,
  update(deco, tr) {
    for (const e of tr.effects) if (e.is(setMarks)) return e.value;
    return deco.map(tr.changes);
  },
  provide: (f) => EditorView.decorations.from(f),
});

export const createImpl = (parent) => (doc) => (onChange) => () => {
  let timer = null;
  return new EditorView({
    parent,
    state: EditorState.create({
      doc,
      extensions: [
        lineNumbers(),
        history(),
        drawSelection(),
        highlightActiveLine(),
        keymap.of([...defaultKeymap, ...historyKeymap, indentWithTab]),
        scriptLanguage,
        syntaxHighlighting(highlight),
        marks,
        EditorView.lineWrapping,
        EditorView.contentAttributes.of({ "aria-label": "Proof script" }),
        EditorView.updateListener.of((u) => {
          if (!u.docChanged) return;
          clearTimeout(timer);
          timer = setTimeout(() => onChange(u.state.doc.toString())(), 250);
        }),
      ],
    }),
  });
};

export const getValue = (view) => () => view.state.doc.toString();

export const setValue = (view) => (text) => () => {
  if (view.state.doc.toString() === text) return;
  view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: text } });
};

// Highlights the current step's line and the error line (0 = none).
export const markLines = (view) => (current) => (error) => (scroll) => () => {
  const doc = view.state.doc;
  const valid = (n) => n > 0 && n <= doc.lines;
  const byLine = new Map();
  if (valid(current)) byLine.set(current, ["cm-current-step"]);
  if (valid(error)) byLine.set(error, [...(byLine.get(error) || []), "cm-error-line"]);
  const builder = new RangeSetBuilder();
  for (const n of [...byLine.keys()].sort((a, b) => a - b)) {
    const l = doc.line(n);
    builder.add(l.from, l.from, Decoration.line({ class: byLine.get(n).join(" ") }));
  }
  const effects = [setMarks.of(builder.finish())];
  if (scroll && valid(current)) {
    effects.push(EditorView.scrollIntoView(doc.line(current).from, { y: "nearest" }));
  }
  view.dispatch({ effects });
};
