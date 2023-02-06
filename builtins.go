package main

import (
	"log"
)

var SymbolNames = []string{
	"quote",
	"unquote",
	"quasiquote",
	"unquote-splicing",
	"...",

	"+",
	"-",
	"*",
	"/",
	">",
	"<",
	"=",

	"not",
	"eqv?",
	"eq?",
	"equal?",

	"car",
	"cdr",
	"set-car!",
	"set-cdr!",
	"display",

	"string=?",
	"symbol->string",
	"char=?",
}

const (
	Quote = Symbol(iota)
	Unquote
	Quasiquote
	UnquoteSplicing
	Elipses

	SymAdd
	SymSub
	SymMul
	SymDiv
	SymGt
	SymLt
	SymEqu

	SymNot
	SymEqv
	SymEq
	SymEqual

	SymCar
	SymCdr
	SymSetCar
	SymSetCdr
	SymDisplay

	SymStringEq
	SymSymbol2String
	SymCharEq
)

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
		SymEqu: &Procedure{builtin: FnNumEq},

		SymNot:   &Procedure{builtin: FnNot},
		SymEqv:   &Procedure{builtin: FnEqv},
		SymEq:    &Procedure{builtin: FnEqv},
		SymEqual: &Procedure{builtin: FnEqual},

		SymCar:     &Procedure{builtin: FnCar},
		SymCdr:     &Procedure{builtin: FnCdr},
		SymSetCar:  &Procedure{builtin: FnSetCar},
		SymSetCdr:  &Procedure{builtin: FnSetCdr},
		SymDisplay: &Procedure{builtin: FnDisplay},

		SymStringEq:      &Procedure{builtin: FnStringEq},
		SymSymbol2String: &Procedure{builtin: FnSymbol2String},
		SymCharEq:        &Procedure{builtin: FnCharEq},
	},
	nil,
}

var Top = &Procedure{}
