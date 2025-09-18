package lib

import "testing"

func TestLib(t *testing.T) {
	t.Run("TestLongestChain", func(t *testing.T) {
		t.Parallel()
		const expected = 837799

		_, ans := LongestChain(1_000_000)

		if ans != expected {
			t.Fatalf("TestLongestChain: wrong answer %d, expected %d", ans, expected)
		}
	})

	t.Run("TestPowerDigitSum", func(t *testing.T) {
		t.Parallel()
		const expected = 1366

		ans := PowerDigitSum()

		if ans != expected {
			t.Fatalf("TestPowerDigitSum: wrong answer %d, expected %d", ans, expected)
		}
	})
}
