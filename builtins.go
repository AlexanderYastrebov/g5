package main

import (
	"errors"
)

var SymbolNames = []string{
	"quote",
	"unquote",
	"quasiquote",
	"unquote-splicing",
	"...",

	"set!",
	"define",
	"lambda",
	"if",
	"define-syntax",
	"save-scope",

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
	"append",

	"display",

	"string=?",
	"symbol->string",
	"char=?",
}

const (
	// Primitives
	Quote = Symbol(iota)
	Unquote
	Quasiquote
	UnquoteSplicing
	Ellipsis

	SymSet
	SymDefine
	SymLambda
	SymIf
	SymDefineSyntax
	SymSaveScope

	// Builtin procedures
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
	SymAppend

	SymDisplay

	SymStringEq
	SymSymbol2String
	SymCharEq

	Last
)

func FnDisplay(nargs int) error {
	if nargs != 1 {
		return errors.New(
			"Wrong arg count to display (ports not yet implemented)")
	}
	return WriteValue(stack.Top(), true, nil)
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
		SymAppend:  &Procedure{builtin: FnAppend},

		SymDisplay: &Procedure{builtin: FnDisplay},

		SymStringEq:      &Procedure{builtin: FnStringEq},
		SymSymbol2String: &Procedure{builtin: FnSymbol2String},
		SymCharEq:        &Procedure{builtin: FnCharEq},
	},
	nil,
}

var Top = &Procedure{macros: map[Symbol]SyntaxRules{}}
