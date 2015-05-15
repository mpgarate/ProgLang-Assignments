interface Node {
  data: number;
  next: Node;
};

interface List {
  add: (e: number) => List;
  remove: () => List;
  foreach: (f: (e: number) => Undefined) => List
};













function newList(): List {
  interface ListRep {
    first: Node;
    add: (e: number) => List;
    remove: () => List;
    foreach: (f: (e: number) => Undefined) => List
  };

  function listClass(this: ListRep): ListRep {
    this.first = null;
    this.add = function(e: number) {
        const n = {data: e, next: this.first};
        this.first = n;
        return this;
      };
    this.remove = function() {
        const n = this.first;
        this.first = this.first.next;
        n.next = null;
        return this;
      };
    this.foreach = function (f: (e: number) => Undefined) {
        function foreach(n: Node): Undefined {
          (n === null) ? undefined : (f(n.data), foreach(n.next));
        };
        foreach(this.first);
        return this;
      };
    return this;
  };
  const list = {
      first: null,
      add: function(e: number): List { return null; },
      remove: function(): List { return null; },
      foreach: function(f: (e: number) => Undefined): List {return null; }
    };
  return listClass(list);
};

const l = newList();
l.add(1).add(4).add(5).foreach(function(e: number) { console.log(e); });



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

interface Hungry (x: number) => Hungry;

function yum(x: number): Hungry {
  return yum;
};

yum(0)(1)(2);
