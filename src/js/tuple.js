Object.defineProperty(globalThis, "fine$tuple", {
  value: function (...items) {
    const obj = {};
    for (const i in items)
      obj[i] = items[i];
    return obj;
  }
});