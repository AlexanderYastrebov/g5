package main

import (
	"errors"
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

func FnCharEq(nargs int) error {
	if nargs != 2 {
		return errors.New("char=? takes 2 arguments")
	}
	stack.Push(Boolean(stack.Pop().(Character) == stack.Pop().(Character)))
	return nil
}
