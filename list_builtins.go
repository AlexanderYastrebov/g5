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

	v := stack.Pop()
	if _, ok := v.(*Pair); !ok {
		return fmt.Errorf("car takes a pair argument, not %T", v)
	}

	if v == Empty {
		stack.Push(Empty)
	} else {
		stack.Push(*v.(*Pair).Car)
	}
	return nil
}

func FnCdr(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to cdr")
	}

	v := stack.Pop()
	if _, ok := v.(*Pair); !ok {
		return fmt.Errorf("cdr takes a pair argument, not %T", v)
	}

	if v == Empty {
		stack.Push(Empty)
	} else {
		stack.Push(*v.(*Pair).Cdr)
	}
	return nil
}

func FnSetCar(nargs int) error {
	if nargs != 2 {
		return errors.New("Wrong arg count to set-car!")
	}

	pair, obj := stack.Pop(), stack.Pop()
	if _, ok := pair.(*Pair); !ok {
		return errors.New("set-car! requires a pair argument")
	}

	if pair == Empty {
		return errors.New("Cannot set-car! on Empty list")
	}

	*pair.(*Pair).Car = obj
	stack.Push(pair)
	return nil
}

func FnSetCdr(nargs int) error {
	if nargs != 2 {
		return errors.New("Wrong arg count to set-cdr!")
	}

	pair, obj := stack.Pop(), stack.Pop()
	if _, ok := pair.(*Pair); !ok {
		return errors.New("set-car! requires a pair argument")
	}

	if pair == Empty {
		return errors.New("Cannot set-cdr! on Empty list")
	}

	*pair.(*Pair).Cdr = obj
	stack.Push(pair)
	return nil
}

func FnApply(nargs int) error {
	if nargs < 2 {
		return errors.New("Wrong arg count to apply")
	}

	proc, ok := stack.Pop().(*Procedure)
	if !ok {
		return errors.New("Got non-procedure for apply")
	}

	args := []Value{}
	for i := 0; i < nargs-2; i++ {
		args = append(args, stack.Pop())
	}

	lastp, ok := stack.Pop().(*Pair)
	if !ok {
		return errors.New("Last argument to apply must be a pair")
	}
	last, err := list2vec(lastp)
	if err != nil {
		return err
	}

	for _, v := range last {
		args = append(args, v)
	}

	for i := len(args) - 1; i >= 0; i-- {
		stack.Push(args[i])
	}
	stack.Push(proc)

	call := Procedure{
		Scope: Top.Scope,
		Ins:   []Ins{{Call, nil, len(args)}},
	}
	return call.Eval()
}

func FnVector2List(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to vector->list")
	}

	l, ok := stack.Pop().(Vector)
	if !ok {
		return errors.New("vector->list takes a vector as the argument")
	}
	stack.Push(vec2list(*l.v))
	return nil
}

func FnString2List(nargs int) error {
	if nargs != 1 {
		return errors.New("string->list takes 1 argument")
	}
	str, ok := stack.Pop().(String)
	if !ok {
		return errors.New("string->list takes a string as the argument")
	}

	v := []Value{}
	rs := []rune(*str.s)
	for i := range rs {
		v = append(v, Char(rs[i]))
	}
	stack.Push(vec2list(v))
	return nil
}

