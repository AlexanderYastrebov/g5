package main

import (
	"errors"
	"reflect"
)

func FnNot(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to not")
	}

	switch val := stack.Pop(); val.(type) {
	case Boolean:
		stack.Push(!val.(Boolean))
	default:
		stack.Push(Boolean(false))
	}
	return nil
}

func FnEqv(nargs int) error {
	obj1, obj2 := stack.Pop(), stack.Pop()

	// The eqv? procedure returns #f if:
	// - obj1 and obj2 are of different types
	if reflect.TypeOf(obj1) != reflect.TypeOf(obj2) {
		stack.Push(Boolean(false))
		return nil
	}

	// The eqv? procedure returns #t if:
	switch obj1.(type) {
	case Boolean, Char, *Procedure:
		// obj1 and obj2 are both #t or both #f.

		// obj1 and obj2 are both characters and are the same character
		// according to the char=? procedure

		// obj1 and obj2 are procedures whose location tags are equal
		stack.Push(Boolean(obj1 == obj2))
		return nil
	case Symbol:
		// obj1 and obj2 are both symbols and
		//
		// (string=? (symbol->string obj1)
		//           (symbol->string obj2))
		//             ===>  #t
		stack.Push(
			Boolean(SymbolNames[obj1.(Symbol)] == SymbolNames[obj2.(Symbol)]))
		return nil
	case Integer, Rational:
		// obj1 and obj2 are both numbers, are numerically equal,
		// and are either both exact or both inexact.
		// **Note: we don't have inexact numbers**
		stack.Push(obj1)
		stack.Push(obj2)
		FnNumEq(2)
		return nil
	case *Pair:
		// both obj1 and obj2 are the empty list.
		if obj1 == Empty && obj2 == Empty {
			stack.Push(Boolean(true))
			return nil
		}
		// obj1 and obj2 are pairs, vectors, or strings that denote the same
		// locations in the store
		stack.Push(Boolean(obj1 == obj2))
		return nil
	}
	stack.Push(Boolean(false))
	return nil
}

func FnEqual(nargs int) error {
	obj1, obj2 := stack.Pop(), stack.Pop()
	stack.Push(Boolean(IsEqual(obj1, obj2)))
	return nil
}
