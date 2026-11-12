// In-browser PDF compilation with typst.ts. The module is imported on the
// first export only, and the compiler (about 28 MB of wasm) and the fonts are
// fetched from jsDelivr at pinned versions, so none of it is in the bundle.
const TYPST_VERSION = "0.7.0";
const WASM_URL = `https://cdn.jsdelivr.net/npm/@myriaddreamin/typst-ts-web-compiler@${TYPST_VERSION}/pkg/typst_ts_web_compiler_bg.wasm`;

let compiler = null;

const getCompiler = () => {
  if (!compiler) {
    compiler = import("@myriaddreamin/typst.ts").then(({ $typst }) => {
      $typst.setCompilerInitOptions({ getModule: () => WASM_URL });
      return $typst;
    });
    compiler.catch(() => {
      compiler = null;
    });
  }
  return compiler;
};

export const compilePdfImpl = (source) => (onError, onSuccess) => {
  getCompiler()
    .then(($typst) => $typst.pdf({ mainContent: source }))
    .then((bytes) => {
      if (!bytes) throw new Error("Typst produced no PDF (see the browser console)");
      onSuccess(bytes);
    })
    .catch(onError);
  return (cancelError, onCancelerError, onCancelerSuccess) => onCancelerSuccess();
};

export const downloadBytes = (filename) => (mime) => (bytes) => () => {
  const url = URL.createObjectURL(new Blob([bytes], { type: mime }));
  const a = document.createElement("a");
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  a.remove();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
};
