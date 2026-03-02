// WebMIDI FFI — thin wrapper around navigator.requestMIDIAccess

export const requestMIDIAccessImpl = function (onSuccess) {
  return function (onError) {
    return function () {
      if (!navigator.requestMIDIAccess) {
        onError(new Error("WebMIDI not supported in this browser"))();
        return;
      }
      navigator.requestMIDIAccess({ sysex: true }).then(
        function (access) { onSuccess(access)(); },
        function (err) { onError(err)(); }
      );
    };
  };
};

export const getOutputsImpl = function (access) {
  return function () {
    var result = [];
    access.outputs.forEach(function (output) {
      result.push({ id: output.id, name: output.name });
    });
    return result;
  };
};

export const getInputsImpl = function (access) {
  return function () {
    var result = [];
    access.inputs.forEach(function (input) {
      result.push({ id: input.id, name: input.name });
    });
    return result;
  };
};

export const openOutputImpl = function (just) {
  return function (nothing) {
    return function (access) {
      return function (portId) {
        return function () {
          var output = null;
          access.outputs.forEach(function (o) {
            if (o.id === portId) output = o;
          });
          return output ? just(output) : nothing;
        };
      };
    };
  };
};

export const openInputImpl = function (just) {
  return function (nothing) {
    return function (access) {
      return function (portId) {
        return function () {
          var input = null;
          access.inputs.forEach(function (i) {
            if (i.id === portId) input = i;
          });
          return input ? just(input) : nothing;
        };
      };
    };
  };
};

export const sendImpl = function (output) {
  return function (bytes) {
    return function () {
      output.send(bytes);
    };
  };
};

export const onMessageImpl = function (input) {
  return function (callback) {
    return function () {
      var handler = function (event) {
        callback(Array.from(event.data))();
      };
      input.onmidimessage = handler;
      return function () {
        input.onmidimessage = null;
      };
    };
  };
};

export const onStateChangeImpl = function (access) {
  return function (callback) {
    return function () {
      var handler = function () { callback(); };
      access.onstatechange = handler;
      return function () {
        access.onstatechange = null;
      };
    };
  };
};

export const randomUUID = function () {
  return crypto.randomUUID();
};
