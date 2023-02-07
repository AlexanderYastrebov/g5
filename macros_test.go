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
		t.Errorf("Could not parse to map: %v", err)
	}

	a := Str2Sym("a")
	if len(m[a]) != 3 {
		t.Errorf("Wrong length for map element: %d vs 3", len(m[a]))
	}

	expected := []Integer{
		Integer(*big.NewInt(1)),
		Integer(*big.NewInt(2)),
		Integer(*big.NewInt(3)),
	}

	for i := range m[a] {
		x := big.Int(m[a][i].(Integer))
		y := big.Int(expected[i])
		if x.Cmp(&y) != 0 {
			t.Errorf("Expected first element to be 1")
		}
	}
}

func TestTranscribe(t *testing.T) {
	pparse := NewParser("(a ...)")
	pval, _ := pparse.GetValue()

	fparse := NewParser("(1 2 3)")
	fval, _ := fparse.GetValue()

	tparse := NewParser("((a ...))")
	tval, _ := tparse.GetValue()

	m := MacroMap{}

	m.parse(pval, fval, []Symbol{})
	res, _ := m.transcribe(tval)

	rparse := NewParser("((1 2 3))")
	rval, _ := rparse.GetValue()

	if !IsEqual(rval, res) {
		t.Errorf("Mismatch")
	}
}


func TestParseSyntaxRules(t *testing.T) {
	input := `(syntax-rules (a b) ((_ b) (cons b a)) ((_ a) (cons a b)))`
	parse := NewParser(input)
	val, err := parse.GetValue()
	if err != nil {
		t.Errorf("Error occurred while parsing input: %v", err)
	}

	valv, _ := list2vec(val.(*Pair))

	result, err := ParseSyntaxRules(valv)
	if err != nil {
		t.Errorf("Error occurred while parsing syntax rules: %v", err)
	}

	expectedLiterals := []Symbol{Str2Sym("a"), Str2Sym("b")}
	for i, literal := range result.Literals {
		if literal != expectedLiterals[i] {
			t.Errorf("Expected literal %v, but got %v",
					expectedLiterals[i], literal)
		}
	}

	expectedPatterns := [][]Value{
		{Str2Sym("_"), Str2Sym("b")},
		{Str2Sym("_"), Str2Sym("a")},
	}
	for i, pattern := range result.Patterns {
		vec, _ := list2vec(pattern)
		for j, value := range vec {
			if value != expectedPatterns[i][j] {
				t.Errorf("Expected pattern %v, but got %v",
					expectedPatterns[i][j], value)
			}
		}
	}

	expectedTemplates := [][]Value{
		{Str2Sym("cons"), Str2Sym("b"), Str2Sym("a")},
		{Str2Sym("cons"), Str2Sym("a"), Str2Sym("b")},
	}
	for i, template := range result.Templates {
		vec, _ := list2vec(template)
		for j, val := range vec {
			if !IsEqual(val, expectedTemplates[i][j]) {
				t.Errorf("Expected template %+v, but got %+v",
						expectedTemplates[i][j], val)
			}
		}
	}
}

