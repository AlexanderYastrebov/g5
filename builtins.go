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
	"*",
	"/",
	">",
	"<",
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

	SymCar
	SymCdr
	SymSetCar
	SymSetCdr
	SymDisplay
)

func FnAdd(nargs int) {
	if nargs == 0 {
		log.Fatalln("Too few args: +")
	}

	total := big.Rat{}

	for nargs > 0 {
		n := stack.Pop()
		nargs--
		n_rat, n_israt := n.(Rational)
		n_int, n_isint := n.(Integer)

		if !n_israt && !n_isint {
			log.Fatalln("Type mismatch: +")
		}

		x := big.Rat{}
		if n_israt {
			x = big.Rat(n_rat)
		} else {
			x_bi := big.Int(n_int)
			x.SetInt(&x_bi)
		}
		total.Add(&total, &x)
	}

	if total.IsInt() {
		stack.Push(Integer(*total.Num()))
		return
	}
	stack.Push(Rational(total))
}

func FnSub(nargs int) {
	total := big.Rat{}
	n := stack.Pop()
	nargs--
	if n_rat, ok := n.(Rational); ok {
		total = big.Rat(n_rat)
	} else {
		n_i, ok := n.(Integer)
		if !ok {
			log.Fatalln("Type mismatch: -")
		}
		n_bi := big.Int(n_i)
		total.SetInt(&n_bi)
	}
	
	if nargs == 0 {
		if total.IsInt() {
			res := total.Num()
			stack.Push(Integer(*res.Neg(res)))
		} else {
			stack.Push(Rational(*total.Neg(&total)))
		}
	}

	for nargs > 0 {
		n := stack.Pop()
		nargs--
		n_rat, n_israt := n.(Rational)
		n_int, n_isint := n.(Integer)

		if !n_israt && !n_isint {
			log.Fatalln("Type mismatch: -")
		}

		x := big.Rat{}
		if n_israt {
			x = big.Rat(n_rat)
		} else {
			x_bi := big.Int(n_int)
			x.SetInt(&x_bi)
		}
		total.Sub(&total, &x)
	}

	if total.IsInt() {
		stack.Push(Integer(*total.Num()))
		return
	}
	stack.Push(Rational(total))
}

func FnMul(nargs int) {
	if nargs == 0 {
		log.Fatalln("Too few args: *")
	}

	total := big.Rat{}
	n := stack.Pop()
	nargs--
	if n_rat, ok := n.(Rational); ok {
		total = big.Rat(n_rat)
	} else {
		n_i, ok := n.(Integer)
		if !ok {
			log.Fatalln("Type mismatch: *")
		}
		n_bi := big.Int(n_i)
		total.SetInt(&n_bi)
	}
	
	if nargs == 0 {
		if total.IsInt() {
			res := total.Num()
			stack.Push(Integer(*res.Neg(res)))
		} else {
			stack.Push(Rational(*total.Neg(&total)))
		}
	}

	for nargs > 0 {
		n := stack.Pop()
		nargs--
		n_rat, n_israt := n.(Rational)
		n_int, n_isint := n.(Integer)

		if !n_israt && !n_isint {
			log.Fatalln("Type mismatch: *")
		}

		x := big.Rat{}
		if n_israt {
			x = big.Rat(n_rat)
		} else {
			x_bi := big.Int(n_int)
			x.SetInt(&x_bi)
		}
		total.Mul(&total, &x)
	}

	if total.IsInt() {
		stack.Push(Integer(*total.Num()))
		return
	}
	stack.Push(Rational(total))
}

func FnDiv(nargs int) {
	if nargs == 0 {
		log.Fatalln("Too few args: /")
	}

	total := big.Rat{}
	n := stack.Pop()
	nargs--
	if n_rat, ok := n.(Rational); ok {
		total = big.Rat(n_rat)
	} else {
		n_i, ok := n.(Integer)
		if !ok {
			log.Fatalln("Type mismatch: /")
		}
		n_bi := big.Int(n_i)
		total.SetInt(&n_bi)
	}
	
	if nargs == 0 {
		if total.IsInt() {
			res := total.Num()
			stack.Push(Integer(*res.Neg(res)))
		} else {
			stack.Push(Rational(*total.Neg(&total)))
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

		x := big.Rat{}
		if n_israt {
			x = big.Rat(n_rat)
		} else {
			x_bi := big.Int(n_int)
			x.SetInt(&x_bi)
		}
		total.Mul(&total, x.Inv(&x))
	}

	if total.IsInt() {
		stack.Push(Integer(*total.Num()))
		return
	}
	stack.Push(Rational(total))
}

func FnGt(nargs int) {
	if nargs == 0 {
		log.Fatalln("Too few args: >")
	}

	last := big.Rat{}
	n := stack.Pop()
	nargs--
	if n_rat, ok := n.(Rational); ok {
		last = big.Rat(n_rat)
	} else {
		n_i, ok := n.(Integer)
		if !ok {
			log.Fatalln("Type mismatch: /")
		}
		n_bi := big.Int(n_i)
		last.SetInt(&n_bi)
	}
	

	for nargs > 0 {
		n := stack.Pop()
		nargs--
		n_rat, n_israt := n.(Rational)
		n_int, n_isint := n.(Integer)

		if !n_israt && !n_isint {
			log.Fatalln("Type mismatch: +")
		}

		x := big.Rat{}
		if !n_israt {
			x_bi := big.Int(n_int)
			x.SetInt(&x_bi)
		} else {
			x = big.Rat(n_rat)
		}

		if last.Cmp(&x) != 1 {
			stack.Push(Boolean(false))
			return
		}
		last = x
	}

	stack.Push(Boolean(true))
}

func FnLt(nargs int) {
	if nargs == 0 {
		log.Fatalln("Too few args: >")
	}

	last := big.Rat{}
	n := stack.Pop()
	nargs--
	if n_rat, ok := n.(Rational); ok {
		last = big.Rat(n_rat)
	} else {
		n_i, ok := n.(Integer)
		if !ok {
			log.Fatalln("Type mismatch: /")
		}
		n_bi := big.Int(n_i)
		last.SetInt(&n_bi)
	}
	

	for nargs > 0 {
		n := stack.Pop()
		nargs--
		n_rat, n_israt := n.(Rational)
		n_int, n_isint := n.(Integer)

		if !n_israt && !n_isint {
			log.Fatalln("Type mismatch: +")
		}

		x := big.Rat{}
		if !n_israt {
			x_bi := big.Int(n_int)
			x.SetInt(&x_bi)
		} else {
			x = big.Rat(n_rat)
		}

		if last.Cmp(&x) != -1 {
			stack.Push(Boolean(false))
			return
		}
		last = x
	}

	stack.Push(Boolean(true))
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

func FnDisplay(nargs int) {
	if nargs != 1 {
		log.Fatalln("Wrong arg count to display (ports not yet implemented)")
	}
	WriteValue(stack.Top(), false, nil)
}

var TopScope = &Scope{
	map[Symbol]Value{
		SymAdd: &Procedure{builtin: FnAdd},
		SymSub: &Procedure{builtin: FnSub},
		SymMul: &Procedure{builtin: FnMul},
		SymDiv: &Procedure{builtin: FnDiv},
		SymGt: &Procedure{builtin: FnGt},
		SymLt: &Procedure{builtin: FnLt},

		SymCar: &Procedure{builtin: FnCar},
		SymCdr: &Procedure{builtin: FnCdr},
		SymSetCar: &Procedure{builtin: FnSetCar},
		SymSetCdr: &Procedure{builtin: FnSetCdr},
		SymDisplay: &Procedure{builtin: FnDisplay},
	},
	nil,
}

var Top = &Procedure{}
