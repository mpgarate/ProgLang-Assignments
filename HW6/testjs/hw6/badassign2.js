var o = { x: { y: 0 } };

const f = function(o: {x: {}}){
    o.x = {  };
  };
  
f(o);
o.x.y
