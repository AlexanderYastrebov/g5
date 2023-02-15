package main

import (
	"fmt"
	"io"
	"math/big"
	"strings"
)

type Value interface {
	isValue()
}

type Boolean bool

func (Boolean) isValue() {}

type Symbol uint

func (Symbol) isValue() {}

// Builtin symbols are listed in builtins.go

type Char rune

func (Char) isValue() {}

type Vector struct {
	v *[]Value
}

func (Vector) isValue() {}

type Scope struct {
	m     map[Symbol]Value
	super *Scope
}

func (*Scope) isValue() {}

type Procedure struct {
	Scope    Scope
	Args     Value
	Ins      []Ins
	Builtin  func(int) error
	CallCC   func(*Procedure, int) error
	Macros   map[Symbol]SyntaxRules

	IsCont    bool
	StackRest *Stack
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

type String struct {
	s *string
}

func (String) isValue() {}

type InputPort struct {
	io.ReadCloser
}

func (InputPort) isValue() {}

type OutputPort struct {
	io.WriteCloser
}

func (OutputPort) isValue() {}

type Scoped struct {
	Symbol Symbol
	Scope  Symbol
}

func (Scoped) isValue() {}

func WriteValue(v Value, display bool) error {
	port := OutputPortStack[len(OutputPortStack) - 1]
	switch v.(type) {
	case Boolean:
		if v.(Boolean) {
			port.Write([]byte("#t"))
		} else {
			port.Write([]byte("#f"))
		}
	case Symbol:
		fmt.Fprint(port, SymbolNames[v.(Symbol)])
	case String:
		if !display {
			fmt.Fprintf(port, "\"%s\"", *v.(String).s)
		} else {
			fmt.Fprint(port, *v.(String).s)
		}
	case Char:
		ch := rune(v.(Char))
		if display {
			fmt.Fprintf(port, "%c", ch)
		} else {
			if ch == '\n' {
				fmt.Fprint(port, "#\\newline")
			} else if ch == ' ' {
				fmt.Fprint(port, "#\\space")
			} else {
				fmt.Fprintf(port, "#\\%c", ch)
			}
		}
	case Vector:
		fmt.Fprint(port, "#(")
		for i, item := range *v.(Vector).v {
			if i != 0 {
				fmt.Fprint(port, " ")
			}
			WriteValue(item, display)
		}
		fmt.Fprint(port, ")")
	case *Pair:
		fmt.Fprint(port, "(")

		cur := v.(*Pair)
		for cur != Empty {
			WriteValue(*cur.Car, display)
			if p, ok := (*cur.Cdr).(*Pair); ok {
				if p != Empty {
					fmt.Fprint(port, " ")
				}
				cur = (*cur.Cdr).(*Pair)
			} else {
				fmt.Fprint(port, " . ")
				WriteValue(*cur.Cdr, display)
				break
			}
		}
		fmt.Fprint(port, ")")

	case Integer:
		i := big.Int(v.(Integer))
		fmt.Fprint(port, i.String())

	case Rational:
		r := big.Rat(v.(Rational))
		fmt.Fprint(port, r.String())

	case *Procedure:
		fmt.Fprint(port, "[procedure]")

	case Procedure:
		fmt.Fprint(port, "[imm_procedure]")

	case *Scope:
		fmt.Fprint(port, "[scope]")

	case Scoped:
		WriteValue(v.(Scoped).Symbol, display)

	default:
		fmt.Fprintf(port, "[??? (%T)]", v)
	}
	return nil
}

func PrintValue(v Value) error {
	return WriteValue(v, false)
}

func Str2Sym(str string) Symbol {
	str = strings.ToLower(str) // Symbols are case-insensitive

	for i, val := range SymbolNames {
		if val == str {
			return Symbol(i)
		}
	}

	SymbolNames = append(SymbolNames, str)
	return Symbol(len(SymbolNames) - 1)
}
