package main

import (
	"errors"
	"math/big"
)

func FnIsVector(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to vector?")
	}

	_, ok := stack.Pop().(Vector)
	stack.Push(Boolean(ok))
	return nil
}

func FnMakeVector(nargs int) error {
	if nargs == 1 && nargs != 2 {
		return errors.New("Wrong arg count to make-vector")
	}

	k, ok := stack.Pop().(Integer)
	if !ok {
		return errors.New("make-vector requires an integer for the first arg")
	}

	k_bi := big.Int(k)

	fill := Empty
	if nargs == 2 {
		fill = stack.Pop()
	}

	vec := Vector{new([]Value)}

	n := int(k_bi.Int64())
	for i := 0; i < n; i++ {
		*vec.v = append(*vec.v, fill)
	}
	stack.Push(vec)
	return nil
}

func FnVectorLength(nargs int) error {
	if nargs != 1 {
		return errors.New("vector-length takes 1 arg")
	}

	vec, ok := stack.Pop().(Vector)
	if !ok {
		return errors.New("vector-length takes a vector as the argument")
	}

	stack.Push(Integer(*big.NewInt(int64(len(*vec.v)))))
	return nil
}

func FnVector(nargs int) error {
	if nargs == 0 {
		return errors.New("vector takes at least 1 args")
	}

	vec := Vector{new([]Value)}
	for i := 0; i < nargs; i++ {
		*vec.v = append(*vec.v, stack.Pop())
	}
	stack.Push(vec)
	return nil
}

func FnVectorRef(nargs int) error {
	if nargs != 2 {
		return errors.New("vector-ref takes 2 args")
	}
	
	vec, ok := stack.Pop().(Vector)
	if !ok {
		return errors.New("vector-ref requires a vector as the first argument")
	}

	idx, ok := stack.Pop().(Integer)
	if !ok {
		return errors.New(
			"vector-ref requires an integer as the second argument")
	}
	idx_bi := big.Int(idx)

	stack.Push((*vec.v)[idx_bi.Int64()])
	return nil
}

func FnVectorSet(nargs int) error {
	if nargs != 3 {
		return errors.New("vector-set! takes 3 args")
	}
	
	vec, ok := stack.Pop().(Vector)
	if !ok {
		return errors.New("vector-set requires a vector as the first argument")
	}

	idx, ok := stack.Pop().(Integer)
	if !ok {
		return errors.New(
			"vector-set requires an integer as the second argument")
	}
	idx_bi := big.Int(idx)

	(*vec.v)[idx_bi.Int64()] = stack.Pop()
	stack.Push(vec)
	return nil
}

func FnList2Vector(nargs int) error {
	if nargs != 1 {
		return errors.New("list->vector takes 1 args")
	}

	p, ok := stack.Pop().(*Pair)
	if !ok {
		return errors.New("list->vector takes a list as the argument")
	}

	v, err := list2vec(p)
	if err != nil {
		return err
	}

	stack.Push(Vector{&v})
	return nil
}
