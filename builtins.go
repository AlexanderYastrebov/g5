package main

import (
	"log"
)

var SymbolNames = []string{
	"quote",
	"unquote",
	"quasiquote",
	"unquote-splicing",

	"+",
	"-",
	"*",
	"/",
	">",
	"<",
	"=",

	"not",

	"car",
	"cdr",
	"set-car!",
	"set-cdr!",
	"display",
}

const (
	Quote = Symbol(iota)
	Unquote
	Quasiquote
	UnquoteSplicing

	SymAdd
	SymSub
	SymMul
	SymDiv
	SymGt
	SymLt
	SymEq

	SymNot

	SymCar
	SymCdr
	SymSetCar
	SymSetCdr
	SymDisplay
)

func FnNot(nargs int) {
	if nargs != 1 {
		log.Fatalln("Wrong arg count to not")
	}

	switch val := stack.Pop(); val.(type) {
	case Boolean:
		stack.Push(!val.(Boolean))
	default:
		stack.Push(Boolean(false))
	}
}

func FnDisplay(nargs int) {
	if nargs != 1 {
		log.Fatalln("Wrong arg count to display (ports not yet implemented)")
	}
	WriteValue(stack.Top(), true, nil)
}

var TopScope = &Scope{
	map[Symbol]Value{
		SymAdd: &Procedure{builtin: FnAdd},
		SymSub: &Procedure{builtin: FnSub},
		SymMul: &Procedure{builtin: FnMul},
		SymDiv: &Procedure{builtin: FnDiv},
		SymGt:  &Procedure{builtin: FnGt},
		SymLt:  &Procedure{builtin: FnLt},
		SymEq:  &Procedure{builtin: FnNumEq},

		SymNot: &Procedure{builtin: FnNot},

		SymCar:     &Procedure{builtin: FnCar},
		SymCdr:     &Procedure{builtin: FnCdr},
		SymSetCar:  &Procedure{builtin: FnSetCar},
		SymSetCdr:  &Procedure{builtin: FnSetCdr},
		SymDisplay: &Procedure{builtin: FnDisplay},
	},
	nil,
}

var Top = &Procedure{}
