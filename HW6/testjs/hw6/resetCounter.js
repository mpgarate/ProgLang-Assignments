interface CounterRep { x: number };

interface Counter {
  get: () => number;
  inc: () => Undefined
};

interface ResetCounter {
  get: () => number;
  inc: () => Undefined;
  reset: () => Undefined;
};

const counterClass = function(rep: CounterRep): Counter {
  return {
    get: function() { return rep.x; },
    inc: function() { rep.x = rep.x + 1; return undefined; }
  }
};

const newCounter = function() {
  const rep = {x: 0};
  return counterClass(rep)
};


const resetCounterClass = function(rep: CounterRep) {
  const _super = counterClass(rep);
  return {
    get: _super.get,
    inc: _super.inc,
    reset: function() { rep.x = 0; return undefined; }
  }
};

const newResetCounter = function() {
  const rep = {x: 0};
  return resetCounterClass(rep);
};

const counterClient = 
  function(c: Counter) {
    c.inc();
    c.inc();
    c.inc();
    return undefined;
  };

const counter = newResetCounter();
console.log(counter.get());
counterClient(counter);
console.log(counter.get());
counter.reset();
console.log(counter.get());
