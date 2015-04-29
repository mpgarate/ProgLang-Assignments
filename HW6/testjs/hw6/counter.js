interface CounterRep {x: number};
interface Counter {
  get: () => number;
  inc: () => Undefined
};

const counterClass = function(rep: CounterRep): Counter {
    return {
      get: function() { return rep.x; },
      inc: function() { rep.x = rep.x + 1; }
    }
  };

const newCounter = function() {
    const rep = {x: 0};
    return counterClass(rep)
  };

const counterClient = 
  function(c: Counter) {
    c.inc();
    c.inc();
    c.inc();
  };

const counter = newCounter();
const counter2 = newCounter();
console.log(counter.get());
counterClient(counter);
console.log(counter.get());
console.log(counter2.get());