// Unload FFI — run an effect as the page goes away.
//
// Both events, on purpose. `beforeunload` is the one that fires on a reload or a
// deliberate navigation; `pagehide` is the one that fires when Safari or a mobile
// browser freezes the page into the back/forward cache, where `beforeunload` may
// not run at all. Sending the same tidy-up twice is harmless — a disconnect to a
// device with no session open is a no-op — whereas missing it leaves an editor
// session held on hardware we can no longer reach.

export const onBeforeUnloadImpl = (effect) => () => {
  const handler = () => {
    // Never let a failure here block the unload. There is nothing useful to do
    // about it and the page is leaving regardless.
    try {
      effect();
    } catch (e) {
      console.error("unload handler failed", e);
    }
  };
  window.addEventListener("beforeunload", handler);
  window.addEventListener("pagehide", handler);
  return () => {
    window.removeEventListener("beforeunload", handler);
    window.removeEventListener("pagehide", handler);
  };
};
