const counterClass = function(rep: {x: number}) {
  return {
    get: function() { return rep.x; },
    inc: function() { rep.x = rep.x + 1; }
  }
};

const newCounter = function() {
  const rep = {x: 0};
  return counterClass(rep)
};


const resetCounterClass = function(rep: {x: number}) {
  const super = counterClass(rep);
  return {
    get: super.get,
    inc: super.inc,
    reset: function() { rep.x = 0; }
  }
};

const newResetCounter = function() {
  const rep = {x: 0};
  return resetCounterClass(rep);
};

const counterClient = 
  function(c: {inc: () => Undefined }) {
    c.inc();
    c.inc();
    c.inc();
  };

const counter = newResetCounter();
console.log(counter.get());
counterClient(counter);
console.log(counter.get());
counter.reset();
console.log(counter.get());
