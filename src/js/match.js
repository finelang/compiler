(function () {
  function Capture(under) { this.under = under; }
  function capture(s) { return new Capture(s); }
  function patternMatch(initX, initPatt) {
    const stack = [[initX, initPatt]];
    const captured = [];
    while (stack.length > 0) {
      const [x, patt] = stack.shift();
      if (patt instanceof Capture) {
        captured.push([patt.under, x]);
        continue;
      }
      if (typeof x !== typeof patt)
        return [false, captured];
      if (typeof x !== "object" || x === null || patt === null) {
        if (x !== patt)
          return [false, captured];
        continue;
      }
      if (!Array.isArray(x) && !Array.isArray(patt)) {
        const pairs = [];
        for (const key in patt) {
          if (!(key in x))
            return [false, captured];
          const innerX = x[key];
          const innerPatt = patt[key];
          pairs.push([innerX, innerPatt]);
        }
        stack.unshift(...pairs);
        continue;
      }
      return [false, captured];
    }
    return [true, captured];
  }
  function match(x, ...matches) {
    for (const [patt, cont] of matches) {
      const [matched, captured] = patternMatch(x, patt);
      if (matched)
        return cont(Object.fromEntries(captured));
    }
    throw Error("No pattern was matched.");
  }
  Object.defineProperties(globalThis, {
    fine$capture: { value: capture },
    fine$match: { value: match }
  });
})();