var Account = function(balance) {
	this.withdraw = function(amount) {
		if (balance >= amount) {
			balance = balance - amount;
			return balance;
		} else {
			return 'Insufficient funds';
		}
	};

	this.deposit = function(amount) {
		balance = balance + amount;
		return balance;
	};
};

