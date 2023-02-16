package main

import (
	"errors"
	"fmt"
	"math/big"
	"math"
	"strings"
	"strconv"
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

func FnIsNumber(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to number?")
	}

	switch v := stack.Pop(); v.(type) {
	case Integer, Rational:
		stack.Push(Boolean(true))
	default:
		stack.Push(Boolean(false))
	}
	return nil
}

func FnIsComplex(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to number?")
	}

	switch v := stack.Pop(); v.(type) {
	case Integer, Rational:
		stack.Push(Boolean(true))
	default:
		stack.Push(Boolean(false))
	}
	return nil
}

func FnIsReal(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to number?")
	}

	switch v := stack.Pop(); v.(type) {
	case Integer, Rational:
		stack.Push(Boolean(true))
	default:
		stack.Push(Boolean(false))
	}
	return nil
}

func FnIsRational(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to number?")
	}

	switch v := stack.Pop(); v.(type) {
	case Integer, Rational:
		stack.Push(Boolean(true))
	default:
		stack.Push(Boolean(false))
	}
	return nil
}

func FnIsInteger(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to number?")
	}

	switch v := stack.Pop(); v.(type) {
	case Integer, Rational:
		stack.Push(Boolean(true))
	default:
		stack.Push(Boolean(false))
	}
	return nil
}

func FnQuotient(nargs int) error {
	if nargs != 2 {
		return errors.New("Wrong arg count to quotient")
	}

	n1, ok1 := stack.Pop().(Integer)
	n2, ok2 := stack.Pop().(Integer)
	if !ok1 || !ok2 {
		return errors.New("quotient takes only integers")
	}

	nb1 := big.Int(n1)
	nb2 := big.Int(n2)
	stack.Push(Integer(*nb1.Quo(&nb1, &nb2)))
	return nil
}

func FnRemainder(nargs int) error {
	if nargs != 2 {
		return errors.New("Wrong arg count to remainder")
	}

	n1, ok1 := stack.Pop().(Integer)
	n2, ok2 := stack.Pop().(Integer)
	if !ok1 || !ok2 {
		return errors.New("remainder takes only integers")
	}

	nb1 := big.Int(n1)
	nb2 := big.Int(n2)
	stack.Push(Integer(*nb1.Rem(&nb1, &nb2)))
	return nil
}

func FnModulo(nargs int) error {
	if nargs != 2 {
		return errors.New("Wrong arg count to modulus")
	}

	n1, ok1 := stack.Pop().(Integer)
	n2, ok2 := stack.Pop().(Integer)
	if !ok1 || !ok2 {
		return errors.New("modulus takes only integers")
	}

	nb1 := big.Int(n1)
	nb2 := big.Int(n2)
	stack.Push(Integer(*nb1.Mod(&nb1, &nb2)))
	return nil
}

func FnNumerator(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to numerator")
	}
	n, ok := stack.Pop().(Rational)
	if !ok {
		return errors.New("numerator only takes rationals")
	}
	nb := big.Rat(n)
	stack.Push(Integer(*nb.Num()))
	return nil
}

func FnDenominator(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to denominator")
	}
	n, ok := stack.Pop().(Rational)
	if !ok {
		return errors.New("denominator only takes rationals")
	}
	nb := big.Rat(n)
	stack.Push(Integer(*nb.Denom()))
	return nil
}

func FnFloor(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to floor")
	}
	n, ok := stack.Pop().(Rational)
	if !ok {
		return errors.New("floor only takes rationals")
	}
	nb := big.Rat(n)

	s := nb.FloatString(1)
	s = s[:len(s)-2]
	res := &big.Int{}
	res.SetString(s, 10)

	if !nb.IsInt() && res.Cmp(big.NewInt(0)) < 0 {
		res.Sub(res, big.NewInt(1))
	}

	stack.Push(Integer(*res))
	return nil
}

func FnCeiling(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to ceiling")
	}
	n, ok := stack.Pop().(Rational)
	if !ok {
		return errors.New("ceiling only takes rationals")
	}
	nb := big.Rat(n)

	s := nb.FloatString(1)
	s = s[:len(s)-2]
	res := &big.Int{}
	res.SetString(s, 10)

	if !nb.IsInt() && res.Cmp(big.NewInt(0)) > 0 {
		res.Add(res, big.NewInt(1))
	}

	stack.Push(Integer(*res))
	return nil
}

func FnTruncate(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to truncate")
	}
	n, ok := stack.Pop().(Rational)
	if !ok {
		return errors.New("truncate only takes rationals")
	}
	nb := big.Rat(n)

	s := nb.FloatString(1)
	s = s[:len(s)-2]
	res := &big.Int{}
	res.SetString(s, 10)

	stack.Push(Integer(*res))
	return nil
}

func FnRound(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to floor")
	}
	n, ok := stack.Pop().(Rational)
	if !ok {
		return errors.New("floor only takes rationals")
	}
	nb := big.Rat(n)

	s := nb.FloatString(0)
	res := &big.Int{}
	res.SetString(s, 10)

	stack.Push(Integer(*res))
	return nil
}

func FnChar2Integer(nargs int) error {
	if nargs != 1 {
		return errors.New("Wrong arg count to char->integer")
	}

	v := stack.Pop()
	c, ok := v.(Char)
	if !ok {
		return fmt.Errorf("Got non-char to char->integer (%T)", v)
	}
	stack.Push(Integer(*big.NewInt(int64(c))))
	return nil
}

func FnRExpt(nargs int) error {
	if nargs != 2 {
		return errors.New("rexpt takes 2 arguments")
	}

	var n float64
	nv := stack.Pop()
	nr, ok := nv.(Rational)
	if ok {
		nbr := big.Rat(nr)
		n, _ = nbr.Float64()
	} else {
		ni, ok := nv.(Integer)
		if !ok {
			return errors.New("rexpt takes a number as the first argument")
		}
		nbi := big.Int(ni)
		n = float64(nbi.Int64())
	}

	pr := stack.Pop().(Rational)
	pbr := big.Rat(pr)
	p, _ := pbr.Float64()

	res := big.Rat{}
	res.SetFloat64(math.Pow(n, p))
	stack.Push(Rational(res))
	return nil
}

func FnLog(nargs int) error {
	if nargs != 1 {
		return errors.New("log takes 1 argument")
	}

	var n float64
	nv := stack.Pop()
	nr, ok := nv.(Rational)
	if ok {
		nbr := big.Rat(nr)
		n, _ = nbr.Float64()
	} else {
		ni, ok := nv.(Integer)
		if !ok {
			return errors.New("log takes a number as the argument")
		}
		nbi := big.Int(ni)
		n = float64(nbi.Int64())
	}

	if n <= 0 {
		return errors.New("logarithm of non-positive number")
	}

	res := big.Rat{}
	res.SetFloat64(math.Log(n))
	stack.Push(Rational(res))
	return nil
}

func FnSin(nargs int) error {
	if nargs != 1 {
		return errors.New("sin takes 1 argument")
	}

	var n float64
	nv := stack.Pop()
	nr, ok := nv.(Rational)
	if ok {
		nbr := big.Rat(nr)
		n, _ = nbr.Float64()
	} else {
		ni, ok := nv.(Integer)
		if !ok {
			return errors.New("sin takes a number as the argument")
		}
		nbi := big.Int(ni)
		n = float64(nbi.Int64())
	}

	res := big.Rat{}
	res.SetFloat64(math.Sin(n))
	stack.Push(Rational(res))
	return nil
}


func FnCos(nargs int) error {
	if nargs != 1 {
		return errors.New("cos takes 1 argument")
	}

	var n float64
	nv := stack.Pop()
	nr, ok := nv.(Rational)
	if ok {
		nbr := big.Rat(nr)
		n, _ = nbr.Float64()
	} else {
		ni, ok := nv.(Integer)
		if !ok {
			return errors.New("cos takes a number as the argument")
		}
		nbi := big.Int(ni)
		n = float64(nbi.Int64())
	}

	res := big.Rat{}
	res.SetFloat64(math.Cos(n))
	stack.Push(Rational(res))
	return nil
}

func FnAsin(nargs int) error {
	if nargs != 1 {
		return errors.New("asin takes 1 argument")
	}

	var n float64
	nv := stack.Pop()
	nr, ok := nv.(Rational)
	if ok {
		nbr := big.Rat(nr)
		n, _ = nbr.Float64()
	} else {
		ni, ok := nv.(Integer)
		if !ok {
			return errors.New("asin takes a number as the argument")
		}
		nbi := big.Int(ni)
		n = float64(nbi.Int64())
	}

	if n < -1 || n > 1 {
		return errors.New("asin argument out of range [-1, 1]")
	}

	res := big.Rat{}
	res.SetFloat64(math.Asin(n))
	stack.Push(Rational(res))
	return nil
}

func FnAcos(nargs int) error {
	if nargs != 1 {
		return errors.New("acos takes 1 argument")
	}

	var n float64
	nv := stack.Pop()
	nr, ok := nv.(Rational)
	if ok {
		nbr := big.Rat(nr)
		n, _ = nbr.Float64()
	} else {
		ni, ok := nv.(Integer)
		if !ok {
			return errors.New("acos takes a number as the argument")
		}
		nbi := big.Int(ni)
		n = float64(nbi.Int64())
	}

	if n < -1 || n > 1 {
		return errors.New("acos argument out of range [-1, 1]")
	}

	res := big.Rat{}
	res.SetFloat64(math.Acos(n))
	stack.Push(Rational(res))
	return nil
}

func FnAtan(nargs int) error {
	if nargs != 1 && nargs != 2 {
		return errors.New("atan takes 1 or 2 arguments")
	}

	var y, x float64
	nx := stack.Pop()
	switch nr := nx.(type) {
	case Rational:
		nbr := big.Rat(nr)
		x, _ = nbr.Float64()
	case Integer:
		nbi := big.Int(nr)
		x = float64(nbi.Int64())
	default:
		return errors.New("atan takes numbers as arguments")
	}
	if nargs == 2 {
		ny := stack.Pop()
		switch nr := ny.(type) {
		case Rational:
			nbr := big.Rat(nr)
			y, _ = nbr.Float64()
		case Integer:
			nbi := big.Int(nr)
			y = float64(nbi.Int64())
		default:
			return errors.New("atan takes numbers as arguments")
		}
	} else {
		x = 1.0
	}

	res := big.Rat{}
	res.SetFloat64(math.Atan2(y, x))
	stack.Push(Rational(res))
	return nil
}


func FnString2Number(nargs int) error {
	if nargs != 1 && nargs != 2 {
		return errors.New("string->number takes 1 or 2 arguments")
	}

	nv := stack.Pop()
	ns, ok := nv.(String)
	if !ok {
		return errors.New("string->number takes a string as the first argument")
	}

	str := *ns.s
	radix := 10
	if nargs == 2 {
		nr, ok := stack.Pop().(Integer)
		if !ok {
			return errors.New("string->number takes an integer as the second argument")
		}

		nbi := big.Int(nr)
		radix = int(nbi.Int64())
	}

	if radix < 2 || radix > 36 {
		return errors.New("invalid radix")
	}

	var num Value
	if strings.Contains(str, "/") {
		// rational number
		parts := strings.Split(str, "/")
		if len(parts) != 2 {
			return errors.New("invalid rational number")
		}
		numerator, err := strconv.ParseInt(parts[0], radix, 64)
		if err != nil {
			return errors.New("failed to convert string to number")
		}
		denominator, err := strconv.ParseInt(parts[1], radix, 64)
		if err != nil {
			return errors.New("failed to convert string to number")
		}
		num = Rational(*big.NewRat(numerator, denominator))
	} else if strings.Contains(str, ".") {
		f, err := strconv.ParseFloat(str, 64)
		if err != nil {
			return errors.New("failed to convert string to number")
		}
		res := big.Rat{}
		res.SetFloat64(f)
		num = Rational(res)
	} else {
		i, err := strconv.ParseInt(str, radix, 64)
		if err != nil {
			return errors.New("failed to convert string to number")
		}
		num = Integer(*big.NewInt(i))
	}

	stack.Push(num)
	return nil
}
