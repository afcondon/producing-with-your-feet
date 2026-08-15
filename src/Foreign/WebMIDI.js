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

// The spec says MIDIPort.id is a DOMString, and Chrome obliges — so the
// PureScript type is String and everything type-checks. The Web MIDI shim
// inside Web MIDI Browser on iOS returns a NUMBER instead, which is a lie the
// type system cannot catch and which fails in the most confusing way possible:
// the <option> value stringifies for display, so the stored selection looks
// character-for-character identical to the port it came from, while `===`
// compares number to string, returns false, and the UI reports "not connected"
// against a port whose id is visibly the same.
//
// So: normalise to String at the boundary, and fall back to the name for
// implementations that omit id altogether.
var portId = function (p) {
  var id = p.id;
  if (id === undefined || id === null || id === "") return String(p.name);
  return String(id);
};

export const getOutputsImpl = function (access) {
  return function () {
    var result = [];
    access.outputs.forEach(function (output) {
      result.push({ id: portId(output), name: output.name });
    });
    return result;
  };
};

export const getInputsImpl = function (access) {
  return function () {
    var result = [];
    access.inputs.forEach(function (input) {
      result.push({ id: portId(input), name: input.name });
    });
    return result;
  };
};

export const openOutputImpl = function (just) {
  return function (nothing) {
    return function (access) {
      return function (wanted) {
        return function () {
          var output = null;
          access.outputs.forEach(function (o) {
            if (portId(o) === wanted || o.name === wanted) output = o;
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
      return function (wanted) {
        return function () {
          var input = null;
          access.inputs.forEach(function (i) {
            if (portId(i) === wanted || i.name === wanted) input = i;
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

// Which port changed, and how. The event carries this and the old binding threw
// it away, so the app could see that *something* had happened but not that the
// MC6 in particular had just come back — and therefore never re-opened it. A
// port that reappears is a different MIDIPort object; the handle held from
// before is dead and will silently deliver nothing.
export const onStateChangeImpl = function (access) {
  return function (callback) {
    return function () {
      var handler = function (ev) {
        var p = ev && ev.port;
        callback({
          id: p && p.id ? p.id : "",
          name: p && p.name ? p.name : "",
          // "connected" | "disconnected"
          state: p && p.state ? p.state : "",
          // "input" | "output"
          portType: p && p.type ? p.type : ""
        })();
      };
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
