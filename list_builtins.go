package main

import (
	"errors"
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
