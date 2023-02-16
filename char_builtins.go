package main

import (
	"errors"
	"fmt"
	"math/big"
	"unicode"
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

func FnCharUpcase(nargs int) error {
	if nargs != 1 {
		return errors.New("char-upcase takes 1 argument")
	}

	cv := stack.Pop()
	c, ok := cv.(Char)
	if !ok {
		return errors.New("char-upcase takes a character as the argument")
	}

	stack.Push(Char(unicode.ToUpper(rune(c))))
	return nil
}

func FnCharDowncase(nargs int) error {
	if nargs != 1 {
		return errors.New("char-downcase takes 1 argument")
	}

	cv := stack.Pop()
	c, ok := cv.(Char)
	if !ok {
		return errors.New("char-downcase takes a character as the argument")
	}

	stack.Push(Char(unicode.ToLower(rune(c))))
	return nil
}
