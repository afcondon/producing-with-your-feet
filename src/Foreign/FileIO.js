// FileIO FFI — browser file download and read

export const downloadJsonImpl = function (filename) {
  return function (jsonString) {
    return function () {
      var blob = new Blob([jsonString], { type: "application/json" });
      var url = URL.createObjectURL(blob);
      var a = document.createElement("a");
      a.href = url;
      a.download = filename;
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    };
  };
};

export const confirmImpl = function (msg) {
  return function () {
    return window.confirm(msg);
  };
};
