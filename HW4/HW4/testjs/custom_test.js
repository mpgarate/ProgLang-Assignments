{
	function fn(n: number): number {
		return n < 100 ? {f: fn(n + 51)}.f : n;
	};

	fn(0);
}