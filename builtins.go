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

	"null?",
	"cons",
	"car",
	"cdr",
	"set-car!",
	"set-cdr!",
	"append",

	"display",

	"string=?",
	"symbol->string",
	"number->string",
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

	SymIsNull
	SymCons
	SymCar
	SymCdr
	SymSetCar
	SymSetCdr
	SymAppend

	SymDisplay

	SymStringEq
	SymSymbol2String
	SymNumber2String
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
		SymAdd: &Procedure{Builtin: FnAdd},
		SymSub: &Procedure{Builtin: FnSub},
		SymMul: &Procedure{Builtin: FnMul},
		SymDiv: &Procedure{Builtin: FnDiv},
		SymGt:  &Procedure{Builtin: FnGt},
		SymLt:  &Procedure{Builtin: FnLt},
		SymEqu: &Procedure{Builtin: FnNumEq},

		SymNot:   &Procedure{Builtin: FnNot},
		SymEqv:   &Procedure{Builtin: FnEqv},
		SymEq:    &Procedure{Builtin: FnEqv},
		SymEqual: &Procedure{Builtin: FnEqual},

		SymIsNull:  &Procedure{Builtin: FnIsNull},
		SymCons:    &Procedure{Builtin: FnCons},
		SymCar:     &Procedure{Builtin: FnCar},
		SymCdr:     &Procedure{Builtin: FnCdr},
		SymSetCar:  &Procedure{Builtin: FnSetCar},
		SymSetCdr:  &Procedure{Builtin: FnSetCdr},
		SymAppend:  &Procedure{Builtin: FnAppend},

		SymDisplay: &Procedure{Builtin: FnDisplay},

		SymStringEq:      &Procedure{Builtin: FnStringEq},
		SymSymbol2String: &Procedure{Builtin: FnSymbol2String},
		SymNumber2String: &Procedure{Builtin: FnNumber2String},
		SymCharEq:        &Procedure{Builtin: FnCharEq},
	},
	nil,
}

var Top = &Procedure{Macros: map[Symbol]SyntaxRules{}}
