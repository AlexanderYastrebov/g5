package main

import (
	"fmt"
	"io"
	"math/big"
	"os"
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

type Character rune

func (Character) isValue() {}

type Vector []Value

func (Vector) isValue() {}

type Scope struct {
	m     map[Symbol]Value
	super *Scope
}

func (*Scope) isValue() {}

type Procedure struct {
	scope   *Scope
	args    Value
	ins     []Ins
	builtin func(int)
	macros  map[Symbol]SyntaxRules
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

type Port struct {
	io.ReadWriteCloser
}

func (Port) isValue() {}

func WriteValue(v Value, display bool, port *Port) {
	var writer io.Writer = port
	if port == nil {
		writer = os.Stdout
	}
	switch v.(type) {
	case Boolean:
		if v.(Boolean) {
			writer.Write([]byte("#t"))
		} else {
			fmt.Print("#f")
		}
	case Symbol:
		fmt.Print(SymbolNames[v.(Symbol)])
	case String:
		if !display {
			fmt.Fprintf(writer, "\"%s\"", v.(String))
		} else {
			fmt.Fprint(writer, v.(String))
		}
	case Character:
		ch := rune(v.(Character))
		if display {
			fmt.Fprintf(writer, "%c", ch)
		} else {
			if ch == '\n' {
				fmt.Fprint(writer, "#\\newline")
			} else if ch == ' ' {
				fmt.Fprint(writer, "#\\space")
			} else {
				fmt.Fprintf(writer, "#\\%c", ch)
			}
		}
	case Vector:
		fmt.Fprint(writer, "#(")
		for i, item := range v.(Vector) {
			if i != 0 {
				fmt.Fprint(writer, " ")
			}
			WriteValue(item, display, port)
		}
		fmt.Fprint(writer, ")")
	case *Pair:
		fmt.Fprint(writer, "(")

		cur := v.(*Pair)
		for {
			if cur == Empty {
				break
			}

			WriteValue(*cur.Car, display, port)

			if p, ok := (*cur.Cdr).(*Pair); ok {
				if p.Car == nil && p.Cdr == nil {
					break
				}
				fmt.Fprint(writer, " ")
				cur = (*cur.Cdr).(*Pair)
			} else {
				fmt.Fprint(writer, " . ")
				WriteValue(*cur.Cdr, display, port)
				break
			}

		}
		fmt.Fprint(writer, ")")

	case Integer:
		i := big.Int(v.(Integer))
		fmt.Fprint(writer, i.String())

	case Rational:
		r := big.Rat(v.(Rational))
		fmt.Fprint(writer, r.String())

	case *Procedure:
		fmt.Fprint(writer, "[procedure]")

	case *Scope:
		fmt.Fprint(writer, "[scope]")

	default:
		fmt.Fprint(writer, "[???]")
	}
}

func PrintValue(v Value) {
	WriteValue(v, true, nil)
}

func Str2Sym(str string) Symbol {
	str = strings.ToLower(str) // Symbols are case-insensitive

	for i, v := range SymbolNames {
		if v == str {
			return Symbol(i)
		}
	}

	SymbolNames = append(SymbolNames, str)
	return Symbol(len(SymbolNames) - 1)
}
