package main

import (
	"errors"
	"fmt"
	"math/big"
	"reflect"
)

type SyntaxRules struct {
	Literals  []Symbol
	Patterns  []*Pair
	Templates []Value
}

func ParseSyntaxRules(v []Value) (*SyntaxRules, error) {
	// (syntax-rules <literals> <syntax-rule> ...)
	if s, ok := v[0].(Symbol); !ok {
		return nil, errors.New("Expected syntax-rules, got non-symbol")
	} else if s != Str2Sym("syntax-rules") {
		return nil, errors.New("Expected syntax-rules, got other symbol")
	}

	litl, ok := v[1].(*Pair)
	if !ok {
		return nil, errors.New("Got non-list for <literals>")
	}

	litv, err := list2vec(litl)
	if err != nil {
		return nil, errors.New("Got improper list for <literals>")
	}

	literals := []Symbol{}
	for _, v := range litv {
		s, ok := v.(Symbol)
		if !ok {
			return nil, errors.New("Got non-symbol in <literals>")
		}
		literals = append(literals, s)
	}

	patterns, templates := []*Pair{}, []Value{}
	for _, v := range v[2:] {
		full, ok := v.(*Pair)
		if !ok {
			return nil, errors.New("Got non-list for <syntax rule>")
		}

		pattern, ok := (*full.Car).(*Pair)
		if !ok {
			return nil, errors.New("Got non-list for <pattern>")
		}

		cdr, ok := (*full.Cdr).(*Pair)
		if !ok {
			return nil, errors.New("Got non-list for <template>")
		}

		templates = append(templates, *cdr.Car)
		patterns = append(patterns, pattern)
	}

	return &SyntaxRules{literals, patterns, templates}, nil
}

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
		if v1 == Empty || v2 == Empty {
			return v1 == Empty && v2 == Empty
		}
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

// pattern, import form
func IsMatch(p Value, f Value, literals []Symbol) bool {
	if IsEqual(p, f) {
		return true
	}

	if p == Empty {
		return false
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
		fp, ok := f.(*Pair)
		if !ok {
			return false
		}

		vp, err := list2vec(p.(*Pair))
		if err != nil { // Dotted list
			var last Value
			cur := p.(*Pair)
			ok := true
			for ok {
				vp = append(vp, *cur.Car)
				last = (*cur.Cdr)
				cur, ok = last.(*Pair)
			}

			vp = append(vp, last)
			vp = append(vp, Ellipsis)
		}

		vf, err := list2vec(fp)
		if err != nil {
			return false
		}

		if vp[len(vp)-1] == Ellipsis {
			// ( a b c ... )

			last := len(vp) - 2
			if len(vf) < last {
				return false
			}

			for i := 0; i < last; i++ {
				if !IsMatch(vp[i], vf[i], literals) {
					return false
				}
			}

			for i := last; i < len(vf); i++ {
				if !IsMatch(vp[last], vf[i], literals) {
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

	return false
}

type MacroList struct {
	v        []Value
	isSingle bool
}
type MacroMap map[Symbol]MacroList

func (m *MacroMap) parse(p Value,
	f Value,
	literals []Symbol,
	isSingle bool) error {
	switch p.(type) {

	case Symbol:
		isliteral := false

		for _, s := range literals {
			if p.(Symbol) == s {
				isliteral = true
				break
			}
		}

		if isliteral && f != nil && p.(Symbol) == f.(Symbol) {
			return nil
		} else if isliteral {
			return errors.New("Macro mismatch: Literal")
		}

		if _, ok := (*m)[p.(Symbol)]; !ok {
			(*m)[p.(Symbol)] = MacroList{[]Value{}, isSingle}
		}
		if f == nil {
			return nil
		}
		res := (*m)[p.(Symbol)]
		res.v = append(res.v, f)
		(*m)[p.(Symbol)] = res
		return nil
	case *Pair:
		vp, err := list2vec(p.(*Pair))
		if err != nil {
			return errors.New("Macro: Non-list")
		}

		var vf []Value
		if f == nil {
			for i := range vp {
				m.parse(vp[i], nil, literals, isSingle)
			}
			return nil
		}
		vf, err = list2vec(f.(*Pair))
		if err != nil {
			return errors.New("Macro mismatch: List vs non-list")
		}

		if len(vp) == 0 && vf != nil {
			if len(vf) == 0 {
				return nil
			} else {
				return errors.New("Macro mismatch: Empty list vs non-empty")
			}
		}

		if vp[len(vp)-1] == Ellipsis && len(vf) >= len(vp)-2 {
			last := len(vp) - 2
			for i := 0; i < last; i++ {
				m.parse(vp[i], vf[i], literals, isSingle)
			}

			m.parse(vp[last], nil, literals, false)
			for i := last; i < len(vf); i++ {
				m.parse(vp[last], vf[i], literals, false)
			}
			return nil
		}

		if len(vf) != len(vp) {
			return fmt.Errorf("Macro mismatch: List length: %d vs %d",
				len(vp), len(vf))
		}

		for i := range vp {
			m.parse(vp[i], vf[i], literals, isSingle)
		}
		return nil
	}
	if IsEqual(p, f) {
		return nil
	}

	return errors.New("Macro mismatch: No match found")
}

func (m *MacroMap) transcribe(t Value, consume bool) (Value, error) {
	switch t.(type) {
	case Symbol:
		vl, ok := (*m)[t.(Symbol)]
		if !ok {
			return t, nil
		}

		if !vl.isSingle && !consume {
			return vec2list(vl.v), nil
		}

		res := vl.v[0]
		if !vl.isSingle && consume {
			res := (*m)[t.(Symbol)]
			res.v = vl.v[1:]
			(*m)[t.(Symbol)] = res
		}
		return res, nil
	case *Pair:
		if t == Empty {
			return Empty, nil
		}

		tp := t.(*Pair)

		if cdr, ok := (*tp.Cdr).(*Pair); ok && cdr != Empty {
			if s, ok := (*cdr.Car).(Symbol); ok && s == Ellipsis {
				key, ok := (*tp.Car).(Symbol)
				if !ok {
					return nil, fmt.Errorf("Currently can only repeat pattern"+
						"variables, got %T", *tp.Car)
				}

				vl, ok := (*m)[key]
				if !ok {
					return nil, fmt.Errorf("Could not find binidng %s",
						SymbolNames[key])

				}

				res := vec2list(vl.v)
				last := res
				for last != Empty && *last.Cdr != Empty {
					last = (*last.Cdr).(*Pair)
				}

				cdr, err := m.transcribe(*cdr.Cdr, false)
				if err != nil {
					return nil, err
				}

				if last == Empty {
					return cdr, nil
				}

				last.Cdr = &cdr
				return res, nil
			}
		}
		car, err := m.transcribe(*tp.Car, false)
		if err != nil {
			return nil, err
		}
		cdr, err := m.transcribe(*tp.Cdr, false)
		if err != nil {
			return nil, err
		}
		return &Pair{&car, &cdr}, nil
	}
	return t, nil
}
