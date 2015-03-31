console.log({x: 3, y: 2}.x);

{
  const o = { 
      x: 3,
      y: "Hello",
      f: function (x: number) { return x + 2 }
  };

  console.log(o.f(o.x));
}
