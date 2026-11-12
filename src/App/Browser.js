// Aff-compatible fetch of a text file (EffectFnAff convention).
export const fetchTextImpl = (url) => (onError, onSuccess) => {
  fetch(url)
    .then((r) => (r.ok ? r.text() : Promise.reject(new Error(`${url}: HTTP ${r.status}`))))
    .then(onSuccess, onError);
  return (cancelError, onCancelerError, onCancelerSuccess) => onCancelerSuccess();
};

export const downloadText = (filename) => (mime) => (text) => () => {
  saveBlob(filename, new Blob([text], { type: mime }));
};

const saveBlob = (filename, blob) => {
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  a.remove();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
};

// Opens a file picker and passes the chosen file's text to the callback.
export const pickTextFile = (accept) => (callback) => () => {
  const input = document.createElement("input");
  input.type = "file";
  input.accept = accept;
  input.addEventListener("change", () => {
    const file = input.files && input.files[0];
    if (file) file.text().then((text) => callback(file.name)(text)());
  });
  input.click();
};

// The examples index written by build.mjs: [{ path, title }]; [] if invalid.
export const parseExamples = (text) => {
  try {
    const xs = JSON.parse(text);
    return Array.isArray(xs)
      ? xs.filter((x) => typeof x.path === "string" && typeof x.title === "string")
      : [];
  } catch (e) {
    return [];
  }
};

// Storage can be missing or throw (private windows, blocked site data).
export const storageGetImpl = (key) => () => {
  try {
    return window.localStorage.getItem(key);
  } catch (e) {
    return null;
  }
};

export const storageSet = (key) => (value) => () => {
  try {
    window.localStorage.setItem(key, value);
  } catch (e) {
    /* ignore */
  }
};

// True when a key event comes from an editable element (the editor, the
// console input), so playback shortcuts should not fire.
export const isTypingTarget = (event) => {
  const t = event.target;
  if (!t || !t.closest) return false;
  return Boolean(t.closest("input, textarea, select, [contenteditable=true], .cm-editor"));
};

// True when the event happened on the element that handles it (and not on a
// child), e.g. a click on a modal's backdrop.
export const isSelfTarget = (event) => event.target === event.currentTarget;

export const preventDefault = (event) => () => event.preventDefault();

export const keyOf = (event) => event.key;

export const scrollToBottom = (element) => () => {
  element.scrollTop = element.scrollHeight;
};

// The position of a pointer or wheel event as fractions of the handling
// element's box (0 at the left/top, 1 at the right/bottom).
const fractions = (event) => {
  const r = event.currentTarget.getBoundingClientRect();
  return {
    fx: r.width > 0 ? (event.clientX - r.left) / r.width : 0.5,
    fy: r.height > 0 ? (event.clientY - r.top) / r.height : 0.5,
  };
};

export const pointerInfo = (event) => ({ id: event.pointerId | 0, ...fractions(event) });

// Wheel delta in pixels (line and page modes converted).
export const wheelInfo = (event) => {
  const scale = event.deltaMode === 1 ? 16 : event.deltaMode === 2 ? 400 : 1;
  return { delta: event.deltaY * scale, ...fractions(event) };
};

export const capturePointer = (event) => () => {
  try {
    event.currentTarget.setPointerCapture(event.pointerId);
  } catch (e) {
    /* ignore */
  }
};
