#ifndef BIT_H_
#define BIT_H_

typedef unsigned long long ull;

inline void update_bit(ull* bit, int size, int idx, ull val) {
	// bit does not support idx 0
	while (idx <= size-1) {
		bit[idx] += val;
		idx += (idx & -idx);
	}
}
inline ull read_prefix_bit(ull* bit, int idx) {
	ull sum = 0ll;
	while (idx > 0) {
		sum += bit[idx];
		idx -= (idx & -idx);
	}
	return sum;
}
inline ull read_bit(ull* bit, int a, int b) {
	return read_prefix_bit(bit, b) - (a > 0 ? read_prefix_bit(bit, a-1) : 0);
}

ull read_single(ull* bit, int idx) {
	ull sum = bit[idx];
	if (idx > 0) {
		int z = idx - (idx & -idx);
		idx--;
		while (idx != z) {
			sum -= bit[idx];
			idx -= (idx & -idx);
		}
	}
	return sum;
}
void set_single(ull* bit, int size, int idx, ull val) {
	update_bit(bit, size, idx, val-read_single(bit, idx));
}


#endif /* BIT_H_ */
