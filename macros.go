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
	for _, val := range litv {
		s, ok := val.(Symbol)
		if !ok {
			return nil, errors.New("Got non-symbol in <literals>")
		}
		literals = append(literals, s)
	}

	patterns, templates := []*Pair{}, []Value{}
	for _, val := range v[2:] {
		full, ok := val.(*Pair)
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

func dot2ellipsis(list *Pair) []Value {
	vp := []Value{}
	var cdr Value
	cur := list
	ok := true
	for ok {
		vp = append(vp, *cur.Car)
		cdr = *cur.Cdr
		cur, ok = cdr.(*Pair)
	}

	vp = append(vp, cdr)
	vp = append(vp, Ellipsis)
	return vp
}

// pattern, import form
func IsMatch(p Value, f Value, literals []Symbol) bool {
	if IsEqual(p, f) {
		return true
	}

	if p == Empty {
		return false
	}

	if _, ok := p.(Scoped); ok {
		p = p.(Scoped).Symbol
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
		if err != nil { // Dotted list
			vp = dot2ellipsis(p.(*Pair))
		}

		pf, ok := f.(*Pair)
		if !ok {
			return false
		}

		vf, err := list2vec(pf)
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
	orig     []Value
}
type MacroMap map[Symbol]*MacroList

func (m *MacroMap) parse(p Value,
	f Value,
	literals []Symbol,
	isSingle bool,
) error {
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
			(*m)[p.(Symbol)] = &MacroList{[]Value{}, isSingle, []Value{}}
		}
		if f == nil {
			return nil
		}
		(*m)[p.(Symbol)].v = append((*m)[p.(Symbol)].v, f)
		(*m)[p.(Symbol)].orig = (*m)[p.(Symbol)].v
		return nil
	case *Pair:
		vp, err := list2vec(p.(*Pair))
		if err != nil {
			vp = dot2ellipsis(p.(*Pair))
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

func (m *MacroMap) transcribe(t Value,
	consume bool,
	name Symbol,
) (Value, error) {
	switch t.(type) {
	case Symbol:
		vl, ok := (*m)[t.(Symbol)]
		if !ok {
			return Scoped{t.(Symbol), name}, nil
		}

		if !vl.isSingle && !consume {
			return vec2list(vl.v), nil
		}

		if len(vl.v) == 0 {
			(*m)[t.(Symbol)].v = (*m)[t.(Symbol)].orig
			return nil, nil
		}

		res := vl.v[0]
		if !vl.isSingle && consume {
			(*m)[t.(Symbol)].v = vl.v[1:]
		}
		return res, nil
	case *Pair:
		if t == Empty {
			return Empty, nil
		}

		tp := t.(*Pair)

		if cdr, ok := (*tp.Cdr).(*Pair); ok && cdr != Empty {
			if s, ok := (*cdr.Car).(Symbol); ok && s == Ellipsis {
				vl := []Value{}
				res, err := m.transcribe(*tp.Car, true, name)
				if err != nil {
					return nil, err
				}
				for res != nil {
					vl = append(vl, res)
					res, err = m.transcribe(*tp.Car, true, name)
					if err != nil {
						return nil, err
					}
				}

				res = vec2list(vl)
				last := res.(*Pair)
				for last != Empty && *last.Cdr != Empty {
					last = (*last.Cdr).(*Pair)
				}

				cdr, err := m.transcribe(*cdr.Cdr, consume, name)
				if cdr == nil {
					return nil, err
				}

				if last == Empty {
					return cdr, nil
				}

				last.Cdr = &cdr
				return res, nil
			}
		}
		car, err := m.transcribe(*tp.Car, consume, name)
		if car == nil {
			return nil, err
		}
		cdr, err := m.transcribe(*tp.Cdr, consume, name)
		if cdr == nil {
			return nil, err
		}
		return &Pair{&car, &cdr}, nil
	}
	return t, nil
}
