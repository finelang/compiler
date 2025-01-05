(function() {
  Object.defineProperty(globalThis, "fine$debug", { value: (x) => (console.debug(x), x) });
})();