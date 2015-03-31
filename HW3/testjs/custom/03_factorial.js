const x = 2;
function plusTwo(y){return y > x ? x + y : x + x; };
function multNeg(y) { return y * (-y); };
function forloop(f) {
  return function reduce(n) {
    return function (acc) {
      return n === 0 ? acc : f(n)(reduce(n - 1)(acc))
    }
  }
};

const factorial =
  function (n) {
    return forloop(function (i) {
      return function (acc) {
        return i * acc
      }
    })(n)(1)
  };
function f(x){return plusTwo(x) + multNeg(x) - factorial(2);};
f(10);
console.log(f(10));