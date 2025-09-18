package lib

import (
	"math/big"
	"strconv"
)

func sumDigits(s string) uint64 {
	sum := uint64(0)
	for _, c := range s {
		n, _ := strconv.Atoi(string(c))
		sum += uint64(n)
	}
	return sum
}

func PowerDigitSum() uint64 {
	n := new(big.Int)
	n.Exp(big.NewInt(2), big.NewInt(1000), nil)
	return sumDigits(n.String())
}
