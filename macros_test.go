package main

import (
	"testing"
	"math/big"
)

func TestBasicMatch(t *testing.T) {
	pparse := NewParser("(a ...)")
	pval, _ := pparse.GetValue()

	fparse := NewParser("(1 2 3)")
	fval, _ := fparse.GetValue()

	if !IsMatch(pval, fval, []Symbol{}) {
		t.Errorf("(a ...) did not match (1 2 3)")
	}
}

func TestLetMatch(t *testing.T) {
	pparse := NewParser("(((a b) ...) body ...)")
	pval, _ := pparse.GetValue()

	fparse := NewParser("(((x 1) (y 2)) (+ 1 1) (+ 1 2))")
	fval, _ := fparse.GetValue()

	if !IsMatch(pval, fval, []Symbol{}) {
		t.Errorf("Match failed on 'let'")
	}
}

func TestBasicMap(t *testing.T) {
	pparse := NewParser("(a ...)")
	pval, _ := pparse.GetValue()

	fparse := NewParser("(1 2 3)")
	fval, _ := fparse.GetValue()

	m := MacroMap{}
	if err := m.parse(pval, fval, []Symbol{}); err != nil {
		t.Errorf("Could not pasre to map: %v", err)
	}

	a := Str2Sym("a")
	if len(m[a]) != 3 {
		t.Errorf("Wrong length for map element: %d vs 3", len(m[a]))
	}

	x := big.Int(m[a][0].(Integer))
	if x.Cmp(big.NewInt(1)) != 0 {
		t.Errorf("Expected first element to be 1")
	}

	x = big.Int(m[a][2].(Integer))
	if x.Cmp(big.NewInt(3)) != 0 {
		t.Errorf("Expected last element to be 3")
	}
}
