// Open a self-contained HTML document in a new tab.
//
// `document.write` rather than a Blob URL or a data: URI, deliberately. A blob
// tab shows a `blob:` address and some print dialogs then refuse to name the
// document; a data: URI is blocked outright for top-level navigation in Chrome.
// Writing into `about:blank` keeps the page same-origin, printable, and
// reloadable, which is all this needs.
//
// The call must happen inside the click that asked for it — a popup opened from
// a later tick is a popup the browser blocks.
export const openSheetImpl = (html) => () => {
  const w = window.open("", "_blank");
  if (!w) return false;
  w.document.open();
  w.document.write(html);
  w.document.close();
  // Nice for the print dialog's default filename and for the tab strip; the
  // <title> in the document does this too, but only after the parser gets there.
  try {
    w.document.title = "Itajara — the board and the recipes";
  } catch (e) {
    // A window that went away between opening and titling is not a failure
    // worth reporting: the document was written.
  }
  return true;
};
