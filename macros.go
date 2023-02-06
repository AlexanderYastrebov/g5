package main

import (
	"math/big"
	"reflect"
	"errors"
)

func IsEqual(v1 Value, v2 Value) bool {
	if reflect.TypeOf(v1) != reflect.TypeOf(v2) {
		return false
	}

	switch v1.(type) {
	case Boolean:
		return v1.(Boolean) == v2.(Boolean)
	case Symbol:
		return SymbolNames[v1.(Symbol)] == SymbolNames[v2.(Symbol)]
	case String:
		return v1.(String) == v2.(String)
	case Character:
		return v1.(Character) == v2.(Character)
	case Vector:
		if len(v1.(Vector)) != len(v2.(Vector)) {
			return false
		}

		for i := range v1.(Vector) {
			if !IsEqual(v1.(Vector)[i], v2.(Vector)[i]) {
				return false
			}
		}
		return true
	case *Pair:
		return IsEqual(*v1.(*Pair).Car, *v2.(*Pair).Car) &&
			IsEqual(*v1.(*Pair).Cdr, *v2.(*Pair).Cdr)
	case Integer:
		b1, b2 := big.Int(v1.(Integer)), big.Int(v2.(Integer))
		return b1.Cmp(&b2) == 0
	case Rational:
		b1, b2 := big.Rat(v1.(Rational)), big.Rat(v2.(Rational))
		return b1.Cmp(&b2) == 0
	case *Procedure:
		return v1.(*Procedure) == v2.(*Procedure)
	case *Scope:
		return v1.(*Scope) == v2.(*Scope)
	default:
		panic("IsEqual: Unknown type")
	}
}

// pattern   input form
func IsMatch(p Value, f Value, literals []Symbol) bool {
	if IsEqual(p, f) {
		return true
	}

	switch p.(type) {
	case Symbol:
		isliteral := false
		for _, s := range literals {
			if p.(Symbol) == s {
				isliteral = true
				break
			}
		}
		return !isliteral || p.(Symbol) == f.(Symbol)
	case *Pair:
		vp, err := list2vec(p.(*Pair))
		if err == nil {
			vf, err := list2vec(f.(*Pair))
			if err != nil {
				return false
			}

			if vp[len(vp) - 1] == Elipses {
				if len(vf) < len(vp) - 1 {
					return false
				}

				for i := 0; i < len(vp) - 1; i++ {
					if !IsMatch(vp[i], vf[i], literals) {
						return false
					}
				}

				return true
			}

			if len(vf) != len(vp) {
				return false
			}

			for i := range vf {
				if !IsMatch(vp[i], vf[i], literals) {
					return false
				}
			}
			return true
		}
	}

	return false
}

type MacroMap map[Symbol][]Value

func getmap(m *MacroMap, p Value, f Value, literals []Symbol) error {
	switch p.(type) {
	case Symbol:
		isliteral := false
		for _, s := range literals {
			if p.(Symbol) == s {
				isliteral = true
				break
			}
		}

		if isliteral && p.(Symbol) == f.(Symbol) {
			return nil
		} else if isliteral {
			return errors.New("Macro mismatch: Literal")
		}

		if _, ok := (*m)[p.(Symbol)]; !ok {
			(*m)[p.(Symbol)] = []Value{}
		}
		(*m)[p.(Symbol)] = append((*m)[p.(Symbol)], f)
		return nil
	case *Pair:
		vp, err := list2vec(p.(*Pair))
		if err == nil {
			vf, err := list2vec(f.(*Pair))
			if err != nil {
				return errors.New("Macro mismatch: List vs non-list")
			}

			if vf[len(vf) - 1] == Elipses && len(vp) >= len(vf) {
				for i := 0; i < len(vp) - 1; i++ {
					getmap(m, vp[i], vf[i], literals)
				}
				for i := len(vp) - 1; i < len(vf); i++ {
					getmap(m, vp[len(vp) - 1], vf[i], literals)
				}
				return nil
			}

			if len(vf) != len(vp) {
				return errors.New("Macro mismatch: List length")
			}
			
			for i := range vp {
				getmap(m, vp[i], vf[i], literals)
			}
			return nil
		}
	}
	if IsEqual(p, f) {
		return nil
	}

	return errors.New("Macro mismatch: No match found")
}

func transcribe(m *MacroMap, t Value) Value {
	switch t.(type) {
	case Symbol:
		vl, ok := (*m)[t.(Symbol)]
		if !ok {
			return t
		}
		res := vl[0]
		if len((*m)[t.(Symbol)]) > 1 {
			(*m)[t.(Symbol)] = (*m)[t.(Symbol)][1:]
		}
		return res
	case *Pair:
		if t == Empty {
			return Empty
		}
		car := transcribe(m, *t.(*Pair).Car)
		cdr := transcribe(m, *t.(*Pair).Cdr)
		return &Pair{&car, &cdr}
	}
	return t
}
