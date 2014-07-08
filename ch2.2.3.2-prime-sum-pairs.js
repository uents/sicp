
var isPrime = function(n) {
	if (n == 1) {
		return true;
	}
	for (var i = 2; i < n; i++) {
		if (n % i == 0) {
			return false;
		}
	}
	return true;
};

var primeSumPairs = function(n) {
	var pairs = [];

	for (var i = 1; i <= n; i++) {
		for (j = 1; j < i; j++) {
			var sum = i + j;
			if (isPrime(sum)) {
				pairs.push([i, j, sum]);
			}
		}
	}
	return pairs;
};
