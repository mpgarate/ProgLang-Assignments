interface Item {
  iName: string;
  iDescription: string;
  iTime: number;
  iAction: () => Item;
};

interface Node {
  data: Item;
  next: Node;
};

interface List {
  add: (n: string, desc: string, t: number, a: () => Item ) => List;
  remove: () => List;
  foreach: (f: (e: Item) => Undefined) => List
};

function newList(): List {
  interface ListRep {
    first: Node;
    add: (e: Item) => List;
    remove: () => List;
    foreach: (f: (e: Item) => Undefined) => List
  };

  function listClass(this: ListRep): ListRep {
    this.first = null;
    this.add = function(n: string, desc: string, t: number, f: () => Item ) {
      const e = {iName: n, iDescription: desc, iTime: t, iAction: f};
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
    this.foreach = function (f: (e: Item) => Undefined) {
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
      add: function(n: string, desc: string, t: number, a: () => Item): List { return null; },
      remove: function(): List { return null; },
      foreach: function(f: (e: Item) => Undefined): List {return null; }
    };
  return listClass(list);
};

const l = newList();
l.add("Make Coffee", "Make the coffee for breakfast", 2, function(){ console.log("brewing coffee"); });
l.foreach(function(e: Item) { e.iAction; } );
