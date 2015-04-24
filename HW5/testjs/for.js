const while = function while(name b: bool): (name body: Undefined) => Undefined  {
    return function (name body: Undefined) {
      return !b ? undefined : (body, while(b)(body))
    }
  };



const for = function for(name init: number, name condition: bool): (name step: Undefined) => (name body: Undefined) => Undefined {
  return function (name step: Undefined) {
    return function (name body: Undefined) {
      init;
      while (condition) ({
        body;
        step;
      });

      return undefined;
    };
  };
};

var i = 0;

for(i = 10, i < 20)({i = 1 + i; undefined})({
  console.log(i);
});
