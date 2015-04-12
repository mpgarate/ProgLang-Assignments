const byvalue = function(const x: number) { return x + x };

const byname = function(name x: number) { return x + x };

const byvar = function(var x: number) { x = x + 1; return x };

const byref = function(ref x: number) { x = x + 1; return x };

var y = 3;

const vy = byvalue(y + 1);
console.log(y);
console.log(vy);

y = 3;

const ny = byname(y + 1);
console.log(y);
console.log(ny);

y = 3;

const vry = byvar(y);
console.log(y);
console.log(vry);

y = 3;

const ry = byvar(y);
console.log(y);
console.log(ry);

