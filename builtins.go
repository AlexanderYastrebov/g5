package main

import (
	"errors"
	"math/big"
	"os"
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

	"exit",

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

	"vector?",
	"make-vector",
	"vector",
	"vector-length",
	"vector-ref",
	"vector-set!",
	"list->vector",

	"write-prim",

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
	SymExit

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

	SymIsVector
	SymMakeVector
	SymVector
	SymVectorLength
	SymVectorRef
	SymVectorSet
	SymList2Vector

	SymWritePrim

	SymStringEq
	SymSymbol2String
	SymNumber2String
	SymCharEq

	SymLast
)

func FnWritePrim(nargs int) error {
	if nargs != 2 {
		return errors.New(
			"Wrong arg count to display-internal (ports not yet implemented)")
	}
	
	isdisplay, ok := stack.Pop().(Boolean)
	if !ok {
		return errors.New("Expected bool as first arg to write-prim")
	}

	return WriteValue(stack.Top(), bool(isdisplay), nil)
}

func FnExit(nargs int) error {
	if nargs > 1 {
		return errors.New("Wrong arg count to exit")
	}
	
	code := 0
	if nargs == 1 {
		v, ok := stack.Pop().(Integer)
		if !ok {
			return errors.New("exit takes an integer as the arg")
		}
		bi := big.Int(v)
		code = int(bi.Int64())
	}

	os.Exit(code)
	return nil
}

var TopScope = &Scope{
	map[Symbol]Value{
		SymExit: &Procedure{Builtin: FnExit},

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

		SymIsVector:     &Procedure{Builtin: FnIsVector},
		SymMakeVector:   &Procedure{Builtin: FnMakeVector},
		SymVector:       &Procedure{Builtin: FnVector},
		SymVectorLength: &Procedure{Builtin: FnVectorLength},
		SymVectorRef:    &Procedure{Builtin: FnVectorRef},
		SymVectorSet:    &Procedure{Builtin: FnVectorSet},
		SymList2Vector:  &Procedure{Builtin: FnList2Vector},

		SymWritePrim: &Procedure{Builtin: FnWritePrim},

		SymStringEq:      &Procedure{Builtin: FnStringEq},
		SymSymbol2String: &Procedure{Builtin: FnSymbol2String},
		SymNumber2String: &Procedure{Builtin: FnNumber2String},
		SymCharEq:        &Procedure{Builtin: FnCharEq},
	},
	nil,
}

var Top = &Procedure{Macros: map[Symbol]SyntaxRules{}}
