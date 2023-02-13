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
	"let-syntax",
	"letrec-syntax",
	"save-scope",
	"syntax-rules",

	"call/cc",
	"exit",

	"+",
	"-",
	"*",
	"/",
	">",
	"<",
	"=",
	"quotient",
	"remainder",
	"modulo",
	"numerator",
	"denominator",
	"floor",
	"ceiling",
	"trunacte",
	"round",
	"char->integer",

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
	"apply",
	"vector->list",

	"vector?",
	"make-vector",
	"vector",
	"vector-length",
	"vector-ref",
	"vector-set!",
	"list->vector",

	"char?",
	"integer->char",

	"write-prim",

	"string=?",
	"symbol->string",
	"number->string",
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
	SymLetSyntax
	SymLetrecSyntax
	SymSaveScope
	SymSyntaxRules

	// Builtin procedures
	SymCallCC
	SymExit

	SymAdd
	SymSub
	SymMul
	SymDiv
	SymGt
	SymLt
	SymEqu
	SymQuotient
	SymRemainder
	SymModulo
	SymNumerator
	SymDenominator
	SymFloor
	SymCeiling
	SymTruncate
	SymRound
	SymChar2Integer

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
	SymApply
	SymVector2List

	SymIsVector
	SymMakeVector
	SymVector
	SymVectorLength
	SymVectorRef
	SymVectorSet
	SymList2Vector

	SymIsChar
	SymInteger2Char

	SymWritePrim

	SymStringEq
	SymSymbol2String
	SymNumber2String

	SymLast
)

func FnCallCC(p *Procedure, nargs int) error {
	p.Cont = true
	proc := stack.Pop()
	vec := []Value{}
	for i := 0; i < nargs; i++ {
		vec = append(vec)
	}
	vec = append(vec, p)

	p.StackPos = len(stack)
	for i := len(vec) - 1; i >= 0; i-- {
		stack.Push(vec[i])
	}

	stack.Push(proc)
	call := Procedure{
		Scope: p.Scope,
		Ins:   []Ins{{Call, nil, nargs}},
	}

	return call.Eval()
}

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

var TopScope = Scope{
	map[Symbol]Value{
		SymCallCC: &Procedure{CallCC: FnCallCC},
		SymExit:   &Procedure{Builtin: FnExit},

		SymAdd:          &Procedure{Builtin: FnAdd},
		SymSub:          &Procedure{Builtin: FnSub},
		SymMul:          &Procedure{Builtin: FnMul},
		SymDiv:          &Procedure{Builtin: FnDiv},
		SymGt:           &Procedure{Builtin: FnGt},
		SymLt:           &Procedure{Builtin: FnLt},
		SymEqu:          &Procedure{Builtin: FnNumEq},
		SymQuotient:     &Procedure{Builtin: FnQuotient},
		SymRemainder:    &Procedure{Builtin: FnRemainder},
		SymModulo:       &Procedure{Builtin: FnModulo},
		SymNumerator:    &Procedure{Builtin: FnNumerator},
		SymDenominator:  &Procedure{Builtin: FnDenominator},
		SymFloor:        &Procedure{Builtin: FnFloor},
		SymCeiling:      &Procedure{Builtin: FnCeiling},
		SymTruncate:     &Procedure{Builtin: FnTruncate},
		SymRound:        &Procedure{Builtin: FnRound},
		SymChar2Integer: &Procedure{Builtin: FnChar2Integer},

		SymNot:   &Procedure{Builtin: FnNot},
		SymEqv:   &Procedure{Builtin: FnEqv},
		SymEq:    &Procedure{Builtin: FnEqv},
		SymEqual: &Procedure{Builtin: FnEqual},

		SymIsNumber:   &Procedure{Builtin: FnIsNumber},
		SymIsComplex:  &Procedure{Builtin: FnIsComplex},
		SymIsReal:     &Procedure{Builtin: FnIsReal},
		SymIsRational: &Procedure{Builtin: FnIsRational},
		SymIsInteger:  &Procedure{Builtin: FnIsInteger},

		SymCons:        &Procedure{Builtin: FnCons},
		SymCar:         &Procedure{Builtin: FnCar},
		SymCdr:         &Procedure{Builtin: FnCdr},
		SymSetCar:      &Procedure{Builtin: FnSetCar},
		SymSetCdr:      &Procedure{Builtin: FnSetCdr},
		SymApply:       &Procedure{Builtin: FnApply},
		SymVector2List: &Procedure{Builtin: FnVector2List},

		SymIsVector:     &Procedure{Builtin: FnIsVector},
		SymMakeVector:   &Procedure{Builtin: FnMakeVector},
		SymVector:       &Procedure{Builtin: FnVector},
		SymVectorLength: &Procedure{Builtin: FnVectorLength},
		SymVectorRef:    &Procedure{Builtin: FnVectorRef},
		SymVectorSet:    &Procedure{Builtin: FnVectorSet},
		SymList2Vector:  &Procedure{Builtin: FnList2Vector},

		SymWritePrim: &Procedure{Builtin: FnWritePrim},

		SymIsChar:       &Procedure{Builtin: FnIsChar},
		SymInteger2Char: &Procedure{Builtin: FnInteger2Char},

		SymStringEq:      &Procedure{Builtin: FnStringEq},
		SymSymbol2String: &Procedure{Builtin: FnSymbol2String},
		SymNumber2String: &Procedure{Builtin: FnNumber2String},
	},
	nil,
}

var Top = &Procedure{Macros: map[Symbol]SyntaxRules{}}
