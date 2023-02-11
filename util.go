package main

import (
	"errors"
)

func vec2list(vec []Value) *Pair {
	if len(vec) == 0 {
		return Empty.(*Pair)
	}

	res := new(Pair)
	cur := res

	for i := range vec {
		v := vec[i]
		cur.Car = &v

		var next Value = new(Pair)

		if i != len(vec)-1 {
			cur.Cdr = &next
			cur = (*cur.Cdr).(*Pair)
		}
	}
	cdr := Empty
	cur.Cdr = &cdr

	return res
}

func list2vec(list *Pair) ([]Value, error) {
	res := []Value{}
	for list != Empty {
		var ok bool
		res = append(res, *list.Car)
		list, ok = (*list.Cdr).(*Pair)
		if !ok {
			return nil, errors.New("Dotted list when regular list expected")
		}
	}
	return res, nil
}

func Unscope(v Value) Value {
	if v == Empty {
		return v
	}
	switch v.(type) {
	case Scoped:
		return v.(Scoped).Symbol
	case *Pair:
		car := Unscope(*v.(*Pair).Car)
		cdr := Unscope(*v.(*Pair).Cdr)
		return &Pair{&car, &cdr}
	default:
		return v
	}
}
