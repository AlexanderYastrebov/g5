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

	"number?",
	"complex?",
	"real?",
	"rational?",
	"integer?",

	"not",
	"eqv?",
	"eq?",
	"equal?",

	"cons",
	"car",
	"cdr",
	"set-car!",
	"set-cdr!",
	"append",
	"apply",

	"display",
	"write",

	"make-vector",

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

	SymIsNumber
	SymIsComplex
	SymIsReal
	SymIsRational
	SymIsInteger

	SymNot
	SymEqv
	SymEq
	SymEqual

	SymCons
	SymCar
	SymCdr
	SymSetCar
	SymSetCdr
	SymAppend
	SymApply

	SymDisplay
	SymWrite

	SymMakeVector

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

func FnWrite(nargs int) error {
	if nargs != 1 {
		return errors.New(
			"Wrong arg count to display (ports not yet implemented)")
	}
	return WriteValue(stack.Top(), false, nil)
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

		SymIsNumber:   &Procedure{Builtin: FnIsNumber},
		SymIsComplex:  &Procedure{Builtin: FnIsComplex},
		SymIsReal:     &Procedure{Builtin: FnIsReal},
		SymIsRational: &Procedure{Builtin: FnIsRational},
		SymIsInteger:  &Procedure{Builtin: FnIsInteger},

		SymCons:   &Procedure{Builtin: FnCons},
		SymCar:    &Procedure{Builtin: FnCar},
		SymCdr:    &Procedure{Builtin: FnCdr},
		SymSetCar: &Procedure{Builtin: FnSetCar},
		SymSetCdr: &Procedure{Builtin: FnSetCdr},
		SymAppend: &Procedure{Builtin: FnAppend},
		SymApply:  &Procedure{Builtin: FnApply},

		SymDisplay: &Procedure{Builtin: FnDisplay},
		SymWrite:   &Procedure{Builtin: FnWrite},

		SymStringEq:      &Procedure{Builtin: FnStringEq},
		SymSymbol2String: &Procedure{Builtin: FnSymbol2String},
		SymNumber2String: &Procedure{Builtin: FnNumber2String},
		SymCharEq:        &Procedure{Builtin: FnCharEq},
	},
	nil,
}

var Top = &Procedure{Macros: map[Symbol]SyntaxRules{}}
