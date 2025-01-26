(function () {
  function withoutProps(obj, props) {
    if (props.length === 0)
      return obj;
    const newObj = Object.fromEntries(Object.entries(obj));
    for (const prop of props)
      delete newObj[prop];
    return newObj;
  }
  function debug(x) {
    console.log(x);
    return x;
  }
  Object.defineProperties(globalThis, {
    fine$withoutProps: { value: withoutProps },
    fine$debug: { value: debug }
  });
})();