interface Foo {
  x: number;
  y: {};
  z: Foo
};

interface Bar {
  x: number;
  y: Bar;
  z: { p: bool; q: Bar }
};

const l = {x: {y: 0}};

const o = (function(): Foo { return {x: 0, y: null, z: null} })();
const p = (function(): Bar { return {x: 1, y: null, z: { p: true, q: null} } })();

const q = true ? o : p;

q.z