export const fetchTextImpl = url => onSuccess => onError => () => {
  fetch(url)
    .then(r => {
      if (!r.ok) throw new Error(`HTTP ${r.status}: ${r.statusText}`);
      return r.text();
    })
    .then(text => onSuccess(text)())
    .catch(err => onError(err)());
};
