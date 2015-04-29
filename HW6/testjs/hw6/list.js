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
l.add(1);
l.add(4);
l.add(5);
l.foreach(function(e: number) { console.log(e); })