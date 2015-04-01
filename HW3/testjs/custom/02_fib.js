function fib(n) {
	return n === 0 ? 0 : (function(n){
		return n === 1 ? 1 : fib(n - 1) + fib(n - 2)
	})(n)
  };

fib(7);

console.log(fib(7));