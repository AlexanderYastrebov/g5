package main

import "testing"

func TestBasicMatch(t *testing.T) {
	pparse := NewParser("(a ...)")
	pval, _ := pparse.GetValue()

	fparse := NewParser("(1 2 3)")
	fval, _ := fparse.GetValue()

	if !IsMatch(pval, fval, []Symbol{}) {
		t.Errorf("(a ...) did not match (1 2 3)")
	}
}
