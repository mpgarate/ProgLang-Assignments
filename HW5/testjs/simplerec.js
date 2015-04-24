var x = 0;
var y = 10;

const fn = function fn(): number {
  x = x + 1;  
  return x > y ? x : fn();
};

fn();