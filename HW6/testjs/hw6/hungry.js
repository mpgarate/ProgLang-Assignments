interface Hungry (x: number) => Hungry;

function yum(x: number): Hungry {
  return yum;
};

yum(0)(1)(2)
