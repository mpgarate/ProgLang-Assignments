const fn = function fn(n1: number): (n2: number) => number {
  return function fn2(n2: number): number {
    return n1 > n2 ? fn(n2 - n1)(n1 - n2) : (n1 * 10) + n2;
  };
};


fn(5)(3);