
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
counterClient(counter);
counter.reset();


interface Hungry (x: number) => Hungry;

function yum(x: number): Hungry {
  console.log("Hungry...");
  counter.inc();
  console.log(counter.get());
  return yum;
};

interface Node {
  data: (x: number) => Hungry;
  next: Node;
};

interface List {
  add: (e: (x: number) => Hungry) => List;
  remove: () => List;
  foreach: (f: (e: (x: number) => Hungry) => Undefined) => List
};

function newList(): List {
  interface ListRep {
    first: Node;
    add: (e: (x: number) => Hungry) => List;
    remove: () => List;
    foreach: (f: (e: (x: number) => Hungry) => Undefined) => List
  };

  function listClass(this: ListRep): ListRep {
    this.first = null;
    this.add = function(e: (x: number) => Hungry) {
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
    this.foreach = function (f: (e: (x: number) => Hungry) => Undefined) {
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
      add: function(e: (x: number) => Hungry): List { return null; },
      remove: function(): List { return null; },
      foreach: function(f: (e: (x: number) => Hungry) => Undefined): List {return null; }
    };
  return listClass(list);
};

const l = newList();

yum(0)(1)(2)(4);
l.add(yum(0));
