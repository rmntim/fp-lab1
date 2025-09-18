package lib

func chainLen(n uint64) uint {
	i := uint(1)
	for ; n > 1; i++ {
		if n%2 == 0 {
			n = n / 2
		} else {
			n = 3*n + 1
		}
	}
	return i
}

func LongestChain(limit uint64) (longestChain uint, longestNum uint64) {
	for i := uint64(2); i < limit; i++ {
		length := chainLen(i)
		if length > longestChain {
			longestChain = length
			longestNum = i
		}
	}
	return
}
