package main

import (
	"math/big"
	"log"
)

var SymbolNames = []string{
	"quote",
	"unquote",
	"quasiquote",
	"unquote-splicing",
	"+",
	"-",
	"car",
	"cdr",
	"set-car!",
	"set-cdr!",
}

var (
	Quote Value           = Symbol(0)
 	Unquote Value         = Symbol(1)
 	Quasiquote Value      = Symbol(2)
	UnquoteSplicing Value = Symbol(3)

	SymAdd     = Symbol(4)
	SymSub     = Symbol(5)
	SymCar     = Symbol(6)
	SymCdr     = Symbol(7)
	SymSetCar  = Symbol(8)
	SymSetCdr  = Symbol(9)
)

func FnAdd(nargs int) {
	sum := big.Rat{}

	for nargs > 0 {
		n := stack.Pop()
		nargs--
		n_rat, n_israt := n.(Rational)
		n_int, n_isint := n.(Integer)

		if !n_israt && !n_isint {
			log.Fatalln("Type mismatch: +")
		}

		if n_israt {
			x := big.Rat(n_rat)
			sum.Add(&sum, &x)
		} else {
			x_bi := big.Int(n_int)
			x := big.Rat{}
			x.SetInt(&x_bi)
			sum.Add(&sum, &x)
		}
	}

	if sum.IsInt() {
		stack.Push(Value(Integer(*sum.Num())))
		return
	}
	stack.Push(Value(Rational(sum)))
}

func FnSub(nargs int) {
	sum := big.Rat{}

	n := stack.Pop()
	nargs--
	if n_rat, ok := n.(Rational); ok {
		sum = big.Rat(n_rat)
	} else {
		n_i, ok := n.(Integer)
		if !ok {
			log.Fatalln("Type mismatch: -")
		}
		n_bi := big.Int(n_i)
		sum.SetInt(&n_bi)
	}
	
	if nargs == 0 {
		if sum.IsInt() {
			res := sum.Num()
			stack.Push(Value(Integer(*res.Neg(res))))
		} else {
			stack.Push(Value(Rational(*sum.Neg(&sum))))
		}
	}

	for nargs > 0 {
		n := stack.Pop()
		nargs--
		n_rat, n_israt := n.(Rational)
		n_int, n_isint := n.(Integer)

		if !n_israt && !n_isint {
			log.Fatalln("Type mismatch: +")
		}

		if n_israt {
			x := big.Rat(n_rat)
			sum.Sub(&sum, &x)
		} else {
			x_bi := big.Int(n_int)
			x := big.Rat{}
			x.SetInt(&x_bi)
			sum.Sub(&sum, &x)
		}
	}

	if sum.IsInt() {
		stack.Push(Value(Integer(*sum.Num())))
		return
	}
	stack.Push(Value(Rational(sum)))
}

func FnCar(nargs int) {
	if nargs != 1 {
		log.Fatalln("Wrong arg count to car")
	}
	p := stack.Pop().(*Pair)
	stack.Push(*p.Car)
}

func FnCdr(nargs int) {
	if nargs != 1 {
		log.Fatalln("Wrong arg count to cdr")
	}
	p := stack.Pop().(*Pair)
	stack.Push(*p.Cdr)
}

func FnSetCar(nargs int) {
	if nargs != 2 {
		log.Fatalln("Wrong arg count to set-car!")
	}
	p := stack.Pop().(*Pair)
	*p.Car = stack.Pop()
}

func FnSetCdr(nargs int) {
	if nargs != 2 {
		log.Fatalln("Wrong arg count to set-cdr!")
	}
	p := stack.Pop().(*Pair)
	*p.Cdr = stack.Pop()
}

var ProcAdd = &Procedure{builtin: FnAdd}
var ProcSub = &Procedure{builtin: FnSub}
var ProcCar = &Procedure{builtin: FnCar}
var ProcCdr = &Procedure{builtin: FnCdr}
var ProcSetCar = &Procedure{builtin: FnSetCar}
var ProcSetCdr = &Procedure{builtin: FnSetCdr}

var TopScope = &Scope{
	map[Symbol]Value{
		SymAdd: ProcAdd,
		SymSub: ProcSub,
		SymCar: ProcCar,
		SymCdr: ProcCdr,
		SymSetCar: ProcSetCar,
		SymSetCdr: ProcSetCdr,
	},
	nil,
}

var Top = &Procedure{}
