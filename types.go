package main

import (
	"fmt"
	"math/big"
)

type Value interface {
	isValue()
}

type Boolean bool
func (Boolean) isValue() {}

type Symbol uint
func (Symbol) isValue() {}
var SymbolNames = []string{"nil", "quote", "lambda"}

var (
	Nil Value = Symbol(0)
	Quote Value = Symbol(1)
 	Lambda Value = Symbol(2)
)

type Character rune
func (Character) isValue() {}

type Vector []Value
func (Vector) isValue() {}

// TODO: Procedures

type Pair struct {
	Car Value
	Cdr *Value
}
func (Pair) isValue() {}

type Number interface {
	Value
	isNum()
}

// No Real or Complex numbers

type Rational interface {
	Number
	isRational()
}
type RationalV big.Rat
func (RationalV) isValue()    {}
func (RationalV) isNum()      {}
func (RationalV) isRational() {}

type Integer interface {
	Number
	isInteger()
}
type IntegerV big.Int
func (IntegerV) isValue()    {}
func (IntegerV) isNum()      {}
func (IntegerV) isRational() {}
func (IntegerV) isInteger()  {}

type String string
func (String) isValue() {}

// TODO: Ports

func PrintValue(v Value) {
	switch v.(type) {
	case Boolean:
		if v.(Boolean) {
			fmt.Print("#t")
		} else {
			fmt.Print("#f")
		}
	case Symbol:
		fmt.Print(SymbolNames[v.(Symbol)])
	case Character:
		fmt.Printf("#\\%c", v.(Character))
	case Vector:
		fmt.Print("#(")
		for i, item := range v.(Vector) {
			if i != 0 {
				fmt.Print(" ")
			}
			PrintValue(item)
		}
		fmt.Print(")")
	case Pair:
		fmt.Print("(")

		cur := v.(Pair)
		for {
			PrintValue(cur.Car)

			if _, ok := (*cur.Cdr).(Pair); ok {
				fmt.Print(" ")
				cur = (*cur.Cdr).(Pair)
			} else if *cur.Cdr == Nil {
				break
			} else {
				fmt.Print(" . ")
				PrintValue(*cur.Cdr)
				break
			}

		}
		fmt.Print(")")
	
	case IntegerV:
		i := big.Int(v.(IntegerV))
		fmt.Print(i.String())

	default:
		fmt.Print("[something]")
	}
}
