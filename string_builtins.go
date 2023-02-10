package main

import (
	"errors"
	"math/big"
)

func FnStringEq(nargs int) error {
	if nargs != 2 {
		return errors.New("string=? takes 2 arguments")
	}
	stack.Push(Boolean(stack.Pop().(String) == stack.Pop().(String)))
	return nil
}

func FnSymbol2String(nargs int) error {
	if nargs != 1 {
		return errors.New("symbol->string takes 1 argument")
	}
	stack.Push(String(SymbolNames[stack.Pop().(Symbol)]))
	return nil
}

func FnNumber2String(nargs int) error {
	if nargs != 1 {
		return errors.New("number->string takes 1 argument")
	}
	v := stack.Pop()

	switch v.(type) {
	case Integer:
		n := big.Int(v.(Integer))
		stack.Push(String(n.String()))
	case Rational:
		n := big.Rat(v.(Rational))
		stack.Push(String(n.String()))
	default:
		return errors.New("number->string takes a numeric argument")
	}
	return nil
}

func FnCharEq(nargs int) error {
	if nargs != 2 {
		return errors.New("char=? takes 2 arguments")
	}
	stack.Push(Boolean(stack.Pop().(Character) == stack.Pop().(Character)))
	return nil
}
