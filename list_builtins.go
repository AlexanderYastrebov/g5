package main

import (
	"errors"
	"fmt"
)

func FnCar(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to car")
	}
	p := stack.Pop().(*Pair)
	stack.Push(*p.Car)
	return nil
}

func FnCdr(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to cdr")
	}
	p := stack.Pop().(*Pair)
	stack.Push(*p.Cdr)
	return nil
}

func FnSetCar(nargs int) error {
	if nargs != 2 {
		return errors.New("Wrong arg count to set-car!")
	}
	p := stack.Pop().(*Pair)
	*p.Car = stack.Pop()
	return nil
}

func FnSetCdr(nargs int) error {
	if nargs != 2 {
		return errors.New("Wrong arg count to set-cdr!")
	}
	p := stack.Pop().(*Pair)
	*p.Cdr = stack.Pop()
	return nil
}

func FnAppend(nargs int) error {
	if nargs == 0 {
		return errors.New("Wrong arg count to append")
	}

	list := stack.Pop()

	p, ok := list.(*Pair)
	if !ok {
		return fmt.Errorf("Expected pair argument to append (got %T)", list)
	}

	vec, err := list2vec(p)
	if err != nil {
		return errors.New("Expected list argument to append")
	}

	for i := 1; i < nargs; i++ {
		vec = append(vec, stack.Pop())
	}
	stack.Push(vec2list(vec))
	return nil
}
