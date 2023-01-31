package main

import (
	"fmt"
	"math/big"
	"io"
	"os"
)

type Value interface {
	isValue()
}

type Boolean bool
func (Boolean) isValue() {}

type Symbol uint
func (Symbol) isValue() {}
// Builtin symbols are listed in builtins.go

type Character rune
func (Character) isValue() {}

type Vector []Value
func (Vector) isValue() {}

type Scope struct {
	m map[Symbol]Value
	super *Scope
}

type Procedure struct {
	scope *Scope
	names *[]Symbol
	ins []Ins
	builtin func(int)
}
func (Procedure) isValue() {}

type Pair struct {
	Car *Value
	Cdr *Value
}
func (*Pair) isValue() {}
var Empty Value = &Pair{nil, nil}

type Rational big.Rat
func (Rational) isValue() {}

type Integer big.Int
func (Integer) isValue() {}

type String string
func (String) isValue() {}

// TODO: Ports

func WriteValue(v Value, quote bool, port io.Writer) {
	if port == nil {
		port = os.Stdout
	}
	switch v.(type) {
	case Boolean:
		if v.(Boolean) {
			fmt.Print("#t")
		} else {
			fmt.Print("#f")
		}
	case Symbol:
		fmt.Print(SymbolNames[v.(Symbol)])
	case String:
		if quote {
			fmt.Printf("\"%s\"", v.(String))
		} else {
			fmt.Print(v.(String))
		}
	case Character:
		ch := rune(v.(Character))

		if ch == '\n' {
			fmt.Print("#\\newline")
		} else if ch == ' ' {
			fmt.Print("#\\space")
		} else {
			fmt.Printf("#\\%c", ch)
		}
	case Vector:
		fmt.Print("#(")
		for i, item := range v.(Vector) {
			if i != 0 {
				fmt.Print(" ")
			}
			WriteValue(item, quote, port)
		}
		fmt.Print(")")
	case *Pair:
		fmt.Print("(")

		cur := v.(*Pair)
		for {
			if (cur.Car == nil) {
				break
			}

			WriteValue(*cur.Car, quote, port)

			if p, ok := (*cur.Cdr).(*Pair); ok {
				if p.Car == nil && p.Cdr == nil {
					break
				}
				fmt.Print(" ")
				cur = (*cur.Cdr).(*Pair)
			} else {
				fmt.Print(" . ")
				WriteValue(*cur.Cdr, quote, port)
				break
			}

		}
		fmt.Print(")")
	
	case Integer:
		i := big.Int(v.(Integer))
		fmt.Print(i.String())

	case Rational:
		r := big.Rat(v.(Rational))
		fmt.Print(r.String())
	
	case Procedure:
		fmt.Print("[procedure]")

	default:
		fmt.Print("[UNHANDLED TYPE]")
	}
}

func PrintValue(v Value) {
	WriteValue(v, true, nil)
}
