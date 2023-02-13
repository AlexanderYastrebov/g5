package main

import (
	"errors"
	"fmt"
	"math/big"
)

func FnIsChar(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to char?")
	}
	_, ok := stack.Pop().(Char)
	stack.Push(Boolean(ok))
	return nil
}

func FnInteger2Char(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to integer->char")
	}
	v := stack.Pop()
	i, ok := v.(Integer)
	if !ok {
		return fmt.Errorf("Got non-char to integer->char (%T)", v)
	}
	bi := big.Int(i)
	stack.Push(Char(rune(bi.Int64())))
	return nil
}
