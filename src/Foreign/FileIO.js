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

export const readFileAsTextImpl = function (accept) {
  return function (onError) {
    return function (onSuccess) {
      return function () {
        var input = document.createElement("input");
        input.type = "file";
        input.accept = accept;
        input.onchange = function (e) {
          var file = e.target.files[0];
          if (!file) {
            onError(new Error("No file selected"))();
            return;
          }
          var reader = new FileReader();
          reader.onload = function (ev) {
            onSuccess(ev.target.result)();
          };
          reader.onerror = function () {
            onError(new Error("Failed to read file"))();
          };
          reader.readAsText(file);
        };
        input.click();
      };
    };
  };
};
