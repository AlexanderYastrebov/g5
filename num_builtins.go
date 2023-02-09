package main

import (
	"math/big"
	"errors"
	"fmt"
)

func FnAdd(nargs int) error {
	if nargs == 0 {
		return errors.New("Too few args: +")
	}

	total := big.Rat{}

	for nargs > 0 {
		n := stack.Pop()
		nargs--
		n_rat, n_israt := n.(Rational)
		n_int, n_isint := n.(Integer)

		if !n_israt && !n_isint {
			return fmt.Errorf("Non-numeric argument to + (%T)", n)
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
		return nil
	}
	stack.Push(Rational(total))
	return nil
}

func FnSub(nargs int) error {
	total := big.Rat{}
	n := stack.Pop()
	nargs--
	if n_rat, ok := n.(Rational); ok {
		total = big.Rat(n_rat)
	} else {
		n_i, ok := n.(Integer)
		if !ok {
			return fmt.Errorf("Non-numeric argument to - (%T)", n)
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
			return fmt.Errorf("Non-numeric argument to - (%T)", n)
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
		return nil
	}
	stack.Push(Rational(total))
	return nil
}

func FnMul(nargs int) error {
	if nargs == 0 {
		return errors.New("Too few args: *")
	}

	total := big.Rat{}
	n := stack.Pop()
	nargs--
	if n_rat, ok := n.(Rational); ok {
		total = big.Rat(n_rat)
	} else {
		n_i, ok := n.(Integer)
		if !ok {
			return fmt.Errorf("Non-numeric argument to * (%T)", n)
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
			return fmt.Errorf("Non-numeric argument to * (%T)", n)
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
		return nil
	}
	stack.Push(Rational(total))
	return nil
}

func FnDiv(nargs int) error {
	if nargs == 0 {
		return errors.New("Too few args: /")
	}

	total := big.Rat{}
	n := stack.Pop()
	nargs--
	if n_rat, ok := n.(Rational); ok {
		total = big.Rat(n_rat)
	} else {
		n_i, ok := n.(Integer)
		if !ok {
			return fmt.Errorf("Non-numeric argument to / (%T)", n)
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
			return fmt.Errorf("Non-numeric argument to / (%T)", n)
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
		return nil
	}
	stack.Push(Rational(total))
	return nil
}

func FnGt(nargs int) error {
	if nargs == 0 {
		stack.Push(Boolean(true))
		return nil
	}

	last := big.Rat{}
	n := stack.Pop()
	nargs--
	if n_rat, ok := n.(Rational); ok {
		last = big.Rat(n_rat)
	} else {
		n_i, ok := n.(Integer)
		if !ok {
			return fmt.Errorf("Non-numeric argument to > (%T)", n)
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
			return fmt.Errorf("Non-numeric argument to > (%T)", n)
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
			return nil
		}
		last = x
	}

	stack.Push(Boolean(true))
	return nil
}

func FnLt(nargs int) error {
	if nargs == 0 {
		stack.Push(Boolean(true))
		return nil
	}

	last := big.Rat{}
	n := stack.Pop()
	nargs--
	if n_rat, ok := n.(Rational); ok {
		last = big.Rat(n_rat)
	} else {
		n_i, ok := n.(Integer)
		if !ok {
			return fmt.Errorf("Non-numeric argument to < (%T)", n)
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
			return fmt.Errorf("Non-numeric argument to < (%T)", n)
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
			return nil
		}
		last = x
	}

	stack.Push(Boolean(true))
	return nil
}

func FnNumEq(nargs int) error {
	if nargs == 0 {
		return errors.New("Too few args: =")
	}

	first_val := stack.Pop()
	nargs--
	first := big.Rat{}

	switch first_val.(type) {
	case Rational:
		first = big.Rat(first_val.(Rational))
	case Integer:
		first_int := big.Int(first_val.(Integer))
		first.SetInt(&first_int)
	default:
		return fmt.Errorf("Non-numeric argument to = (%T)\n", first_val)
	}

	for nargs > 0 {
		n_val := stack.Pop()
		nargs--
		n := big.Rat{}

		switch n_val.(type) {
		case Rational:
			n = big.Rat(n_val.(Rational))
		case Integer:
			n_int := big.Int(n_val.(Integer))
			n.SetInt(&n_int)
		default:
			return fmt.Errorf("Non-numeric argument to = (%T)\n", n_val)
		}

		if first.Cmp(&n) != 0 {
			stack.Push(Boolean(false))
			return nil
		}
	}

	stack.Push(Boolean(true))
	return nil
}
