(function () {
  function Capture(under) { this.under = under; }
  function CaptureMany(under) { this.under = under; }
  function capture(s) { return new Capture(s); }
  function captureObj(s) { return { [Symbol()]: new CaptureMany(s) }; }
  function objPatternMatch(x, patt) {
    const sym = Object.getOwnPropertySymbols(patt)
      .find(sym => patt[sym] instanceof CaptureMany);
    const cmPatt = sym && patt[sym];
    const pairs = [];
    for (const key in patt)
      pairs.push([x[key], patt[key]]);
  
    if (cmPatt) {
      const obj = {};
      for (const key in x)
        if (!(key in patt))
          obj[key] = x[key];
      pairs.push([obj, new Capture(cmPatt.under)])
    }
    return pairs;
  }
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
        stack.unshift(...objPatternMatch(x, patt));
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
    fine$captureObj: { value: captureObj },
    fine$match: { value: match }
  });
})();