interface Node {
  data: number;
  left: Node;
  right: Node;
}

interface Tree {
  add: (e: number) => Tree;
  remove: (e: number) => Tree;
  foreachInOrder: ( f: (e: number) => Undefined) => Tree;
}



function newTree(): Tree {
  interface TreeRep {
    oldParent: Node;
    add: (e: number) => Tree;
    remove: (e: number) => Tree;
    foreachInOrder: ( f: (e: number) => Undefined) => Tree;
  };

  function treeClass(this: TreeRep): ListRep {
    this.oldParent = null;
  }
}
