(() => {
  const $RETURN = Symbol();
  const $HANDLER = Symbol();

  function isFunction(x) {
    return typeof x === "function";
  }
  
  function isNullish(x) {
    return x === null || typeof x === "undefined";
  }

  function isGenerator(x) {
    return !isNullish(x) && isFunction(x.next);
  }

  function runGenerator(gen, genArg) {
    while (true) {
      const { value, done } = gen.next(genArg);

      if (done) {
        const ret = gen[$RETURN];
        if (isGenerator(ret)) {
          gen = ret; genArg = value;
          continue;
        }
        if (isFunction(ret))
          return ret(value);
        return;
      }

      if (isGenerator(value)) {
        value[$RETURN] = gen;
        gen = value; genArg = null;
        continue;
      }
      if (isFunction(value))
        return value(gen);
      return;
    }
  }

  function withHandler(handler, gen) {
    const handlerGen = (function* () {
      const result = yield gen;
      
      const ret = handler[$RETURN];
      if (!isNullish(ret) && isFunction(ret))
        return yield ret(result);

      return result;
    })();
    handlerGen[$HANDLER] = handler;

    return handlerGen;
  }

  function closestHandlerGen(effectName, gen) {
    while (true) {
      const handler = gen[$HANDLER];
      if (!isNullish(handler) && handler.hasOwnProperty(effectName))
        return gen;

      const ret = gen[$RETURN];
      if (isNullish(ret))
        return null;

      gen = ret;
    }
  }

  function runEffect(effectName, ...args) {
    return runEffectGen => {

      const handlerGen = closestHandlerGen(effectName, runEffectGen);
      if (isNullish(handlerGen))
        throw new Error();
  
      const handler = handlerGen[$HANDLER][effectName];
  
      const gen = handler(function resume(value) {
        return currentGen => {
          handlerGen[$RETURN] = currentGen;
          runGenerator(runEffectGen, value);
        };
      }, ...args);

      gen[$RETURN] = handlerGen[$RETURN];
      runGenerator(gen, null);
    };
  }

  function run(gen, onDone) {
    gen[$RETURN] = onDone;
    runGenerator(gen, null);
  }

  Object.defineProperties(globalThis, {
    $runEffect: { value: runEffect },
    $withHandler: { value: withHandler },
    $run: { value: run }
  });
})();
