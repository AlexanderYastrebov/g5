package main

import (
	"math/big"
	"log"
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

var ProcAdd = Procedure{super: Top, builtin: FnAdd}

var TopScope = Scope{
	map[Symbol]Value{
		SymAdd: &ProcAdd,
	},
	nil,
}

var Top = &Procedure{}
