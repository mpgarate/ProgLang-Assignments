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
// parameter type is the meet of T and S
true ? function(x: T) {return null} : function(x: S) {return null}
