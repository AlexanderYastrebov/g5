package main

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
	"dynamic-wind",
	"values",
	"call-with-values",

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
	"string->list",

	"vector?",
	"make-vector",
	"vector",
	"vector-length",
	"vector-ref",
	"vector-set!",
	"list->vector",

	"char?",
	"integer->char",

	"string?",
	"make-string",
	"string",
	"string-length",
	"string-ref",
	"string-set!",
	"string-downcase",
	"substring",
	"string-append",
	"string=?",
	"symbol->string",
	"number->string",
	"list->string",

	"port?",
	"call-with-input-file",
	"call-with-output-file",
	"open-input-file",
	"open-output-file",
	"close-input-port",
	"close-output-port",
	"read",
	"read-char",
	"peek-char",
	"eof-object?",
	"char-ready?",
	"write",
	"display",

	"procedure?",

	"get-environment-variables",
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
	SymDynamicWind
	SymValues
	SymCallWithValues

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
	SymString2List

	SymIsVector
	SymMakeVector
	SymVector
	SymVectorLength
	SymVectorRef
	SymVectorSet
	SymList2Vector

	SymIsChar
	SymInteger2Char

	SymIsString
	SymMakeString
	SymString
	SymStringLength
	SymStringRef
	SymStringSet
	SymStringDowncase
	SymSubstring
	SymStringAppend
	SymStringEq
	SymSymbol2String
	SymNumber2String
	SymList2String

	SymIsPort
	SymCallWithInputFile
	SymCallWithOutputFile
	SymOpenInputFile
	SymOpenOutputFile
	SymCloseInputPort
	SymCloseOutputPort
	SymRead
	SymReadChar
	SymPeekChar
	SymIsEofObject
	SymIsCharReady
	SymWrite
	SymDisplay


	SymIsProcedure

	SymGetEnvironmentVariables

	SymLast
)

var TopScope = Scope{
	map[Symbol]Value{
		SymCallCC:         &Procedure{CallCC: FnCallCC},
		SymExit:           &Procedure{Builtin: FnExit},
		SymDynamicWind:    &Procedure{Builtin: FnDynamicWind},
		SymValues:         &Procedure{Builtin: FnValues},
		SymCallWithValues: &Procedure{Builtin: FnCallWithValues},

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
		SymString2List: &Procedure{Builtin: FnString2List},

		SymIsVector:     &Procedure{Builtin: FnIsVector},
		SymMakeVector:   &Procedure{Builtin: FnMakeVector},
		SymVector:       &Procedure{Builtin: FnVector},
		SymVectorLength: &Procedure{Builtin: FnVectorLength},
		SymVectorRef:    &Procedure{Builtin: FnVectorRef},
		SymVectorSet:    &Procedure{Builtin: FnVectorSet},
		SymList2Vector:  &Procedure{Builtin: FnList2Vector},

		SymIsChar:       &Procedure{Builtin: FnIsChar},
		SymInteger2Char: &Procedure{Builtin: FnInteger2Char},

		SymIsString:       &Procedure{Builtin: FnIsString},
		SymMakeString:     &Procedure{Builtin: FnMakeString},
		SymString:         &Procedure{Builtin: FnString},
		SymStringLength:   &Procedure{Builtin: FnStringLength},
		SymStringRef:      &Procedure{Builtin: FnStringRef},
		SymStringSet:      &Procedure{Builtin: FnStringSet},
		SymStringDowncase: &Procedure{Builtin: FnStringDowncase},
		SymSubstring:      &Procedure{Builtin: FnSubstring},
		SymStringAppend:   &Procedure{Builtin: FnStringAppend},
		SymStringEq:       &Procedure{Builtin: FnStringEq},
		SymSymbol2String:  &Procedure{Builtin: FnSymbol2String},
		SymNumber2String:  &Procedure{Builtin: FnNumber2String},
		SymList2String:    &Procedure{Builtin: FnList2String},

		SymIsPort:             &Procedure{Builtin: FnIsPort},
		SymCallWithInputFile:  &Procedure{Builtin: FnCallWithInputFile},
		SymCallWithOutputFile: &Procedure{Builtin: FnCallWithOutputFile},
		SymOpenInputFile:      &Procedure{Builtin: FnOpenInputFile},
		SymOpenOutputFile:     &Procedure{Builtin: FnOpenOutputFile},
		SymCloseInputPort:     &Procedure{Builtin: FnCloseInputPort},
		SymCloseOutputPort:    &Procedure{Builtin: FnCloseOutputPort},
		SymRead:               &Procedure{Builtin: FnRead},
		SymReadChar:           &Procedure{Builtin: FnReadChar},
		SymPeekChar:           &Procedure{Builtin: FnPeekChar},
		SymIsEofObject:        &Procedure{Builtin: FnIsEofObject},
		SymWrite:              &Procedure{Builtin: FnWrite},
		SymDisplay:            &Procedure{Builtin: FnDisplay},

		SymIsProcedure: &Procedure{Builtin: FnIsProcedure},

		SymGetEnvironmentVariables: &Procedure{
			Builtin: FnGetEnvironmentVariables,
		},
	},
	nil,
}

var Top = &Procedure{Macros: map[Symbol]SyntaxRules{}}
