package main

import (
	"errors"
	"fmt"
)

func FnCons(nargs int) error {
	if nargs != 2 {
		return errors.New("Wrong arg count to cons")
	}
	obj1 := stack.Pop()
	obj2 := stack.Pop()
	stack.Push(&Pair{&obj1, &obj2})
	return nil
}

func FnCar(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to car")
	}
	p := stack.Pop().(*Pair)
	if p == Empty {
		stack.Push(Empty)
	} else {
		stack.Push(*p.Car)
	}
	return nil
}

func FnCdr(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to cdr")
	}
	p := stack.Pop().(*Pair)
	if p == Empty {
		stack.Push(Empty)
	} else {
		stack.Push(*p.Cdr)
	}
	return nil
}

func FnSetCar(nargs int) error {
	if nargs != 2 {
		return errors.New("Wrong arg count to set-car!")
	}

	pair := stack.Pop()
	obj := stack.Pop()

	if _, ok := pair.(*Pair); !ok {
		return errors.New("set-car! requires a pair argument")
	}

	pair.(*Pair).Car = &obj
	stack.Push(pair)
	return nil
}

func FnSetCdr(nargs int) error {
	if nargs != 2 {
		return errors.New("Wrong arg count to set-cdr!")
	}

	pair := stack.Pop()
	obj := stack.Pop()

	if _, ok := pair.(*Pair); !ok {
		return errors.New("set-car! requires a pair argument")
	}

	pair.(*Pair).Cdr = &obj
	stack.Push(pair)
	return nil
}

func FnAppend(nargs int) error {
	if nargs == 0 {
		return errors.New("Wrong arg count to append")
	}

	vec := []Value{}
	for i := nargs; i > 1; i-- {
		v := stack.Pop()
		p, ok := v.(*Pair)
		if !ok {
			return fmt.Errorf("Expected pair argument to append (got %T)", v)
		}

		next, err := list2vec(p)
		if err != nil {
			return errors.New(
				"Expected list argument to append, got improper list")
		}

		for _, v := range next {
			vec = append(vec, v)
		}
	}
	if len(vec) == 0 {
		return nil
	}
	newp := vec2list(vec)

	cur, last := Value(newp), Value(newp)
	for cur != Empty {
		last = cur
		if _, ok := cur.(*Pair); !ok {
			return errors.New("Got improper list for first arg to append")
		}
		cur = *cur.(*Pair).Cdr
	}

	cdr := stack.Pop()
	last.(*Pair).Cdr = &cdr
	stack.Push(newp)
	return nil
}

func FnApply(nargs int) error {
	if nargs < 2 {
		return errors.New("Wrong arg count to apply")
	}

	p, ok := stack.Pop().(*Procedure)
	if !ok {
		return errors.New("Got non-procedure for apply")
	}

	vec := []Value{}
	for i := 0; i < nargs-2; i++ {
		vec = append(vec, stack.Pop())
	}

	l, ok := stack.Pop().(*Pair)
	if !ok {
		return errors.New("Last argument to apply must be a pair")
	}
	lv, err := list2vec(l)
	if err != nil {
		return err
	}

	for _, v := range lv {
		vec = append(vec, v)
	}

	for i := len(vec) - 1; i >= 0; i-- {
		stack.Push(vec[i])
	}

	stack.Push(p)
	call := Procedure{
		Scope: Top.Scope,
		Ins:   []Ins{{Call, nil, len(vec)}},
	}
	return call.Eval()
}
