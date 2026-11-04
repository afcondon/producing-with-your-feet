// `?midi=hub` and nothing else turns the hub on; see MidiTransport.purs.
export const askedForHubImpl = () => {
  try {
    return new URLSearchParams(location.search).get("midi") === "hub";
  } catch (e) {
    return false;
  }
};

export const hostnameImpl = () => location.hostname || "localhost";
