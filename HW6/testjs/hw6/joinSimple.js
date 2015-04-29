interface T {
  x: number;
  y: {w: bool};
  z: number
};

interface S {
  x: number;
  y: {w: number}
};

// The type of the following expression is a function type whose
// return type is the join of T and S
true ? function():T {return null} : function():S {return null}
