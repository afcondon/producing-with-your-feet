// Remote FFI — talking to pwyf-store.
//
// The base URL is derived from wherever the app itself was served from, so the
// same bundle works unchanged whether that is localhost, a LAN address, or a
// Tailscale name like andrews-mac-mini. Hardcoding an IP would break every time
// the LAN reallocates; deriving it means the store is always "the same host I
// came from, on 3002".
//
// Override with:  localStorage.setItem('pwyf-store-url', 'http://host:3002')

const STORE_PORT = 3002;

export const storeBaseUrlImpl = function () {
  try {
    const override = localStorage.getItem("pwyf-store-url");
    if (override) return override;
  } catch (e) {
    // localStorage can throw in hardened privacy modes; fall through.
  }
  const proto = location.protocol === "https:" ? "https:" : "http:";
  const host = location.hostname || "localhost";
  return proto + "//" + host + ":" + STORE_PORT;
};

export const requestImpl = function (method) {
  return function (url) {
    return function (body) {
      return function (onError) {
        return function (onSuccess) {
          return function () {
            const opts = { method: method };
            if (method !== "GET") {
              opts.headers = { "Content-Type": "application/json" };
              opts.body = body;
            }
            fetch(url, opts)
              .then(function (res) {
                return res.text().then(function (txt) {
                  if (res.ok) {
                    onSuccess(txt)();
                  } else {
                    // The 409 from the store's wipe guard arrives here, and its
                    // body says what it refused to delete — keep it intact.
                    onError(new Error("HTTP " + res.status + ": " + txt))();
                  }
                });
              })
              .catch(function (e) {
                onError(e)();
              });
          };
        };
      };
    };
  };
};
