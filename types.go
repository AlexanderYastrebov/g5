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
	Scope   Scope
	Args    Value
	Ins     []Ins
	Builtin func(int) error
	Macros  map[Symbol]SyntaxRules
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

type Scoped struct {
	Symbol Symbol
	Scope  Symbol
}

func (Scoped) isValue() {}

func WriteValue(v Value, display bool, port *Port) error {
	var writer io.Writer = port
	if port == nil {
		writer = os.Stdout
	}
	switch v.(type) {
	case Boolean:
		if v.(Boolean) {
			writer.Write([]byte("#t"))
		} else {
			writer.Write([]byte("#f"))
		}
	case Symbol:
		fmt.Fprint(writer, SymbolNames[v.(Symbol)])
	case String:
		if !display {
			fmt.Fprintf(writer, "\"%s\"", v.(String))
		} else {
			fmt.Fprint(writer, v.(String))
		}
	case Char:
		ch := rune(v.(Char))
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
		for i, item := range *v.(Vector).v {
			if i != 0 {
				fmt.Fprint(writer, " ")
			}
			WriteValue(item, display, port)
		}
		fmt.Fprint(writer, ")")
	case *Pair:
		fmt.Fprint(writer, "(")

		cur := v.(*Pair)
		for cur != Empty {
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

	case Procedure:
		fmt.Fprint(writer, "[imm_procedure]")

	case *Scope:
		fmt.Fprint(writer, "[scope]")

	case Scoped:
		WriteValue(v.(Scoped).Symbol, display, port)

	default:
		fmt.Fprintf(writer, "[??? (%T)]", v)
	}
	return nil
}

func PrintValue(v Value) error {
	return WriteValue(v, false, nil)
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
