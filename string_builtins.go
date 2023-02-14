package main

import (
	"errors"
	"math/big"
	"strings"
)

func FnIsString(nargs int) error {
	if nargs != 1 {
		return errors.New("string? takes 1 argument")
	}
	_, ok := stack.Pop().(String)
	stack.Push(Boolean(ok))
	return nil
}

func FnMakeString(nargs int) error {
	if nargs != 1 && nargs != 2 {
		return errors.New("make-string takes 1 or 2 arguments")
	}
	ch := "X"
	k_v := stack.Pop().(Integer)
	k_bi := big.Int(k_v)
	k := int(k_bi.Int64())

	if nargs == 2 {
		ch_v, ok := stack.Pop().(Char)
		if !ok {
			return errors.New("Second argument to make-string must be a char")
		}
		ch = string(ch_v)
	}

	s := ""
	for i := 0; i < k; i++ {
		s += ch
	}
	stack.Push(String{&s})
	return nil
}

func FnString(nargs int) error {
	if nargs == 0 {
		return errors.New("string takes at least 1 argument")
	}

	s := ""
	for i := 0; i < nargs; i++ {
		ch, ok := stack.Pop().(Char)
		if !ok {
			return errors.New("string takes chars as arguments")
		}
		s += string(ch)
	}
	stack.Push(String{&s})
	return nil
}

func FnStringLength(nargs int) error {
	if nargs != 1 {
		return errors.New("string-length takes 1 argument")
	}
	s, ok := stack.Pop().(String)
	if !ok {
		return errors.New("string-length takes a string as the argument")
	}
	stack.Push(Integer(*big.NewInt(int64(len(*s.s)))))
	return nil
}

func FnStringRef(nargs int) error {
	if nargs != 2 {
		return errors.New("string-ref takes 2 arguments")
	}
	s, ok := stack.Pop().(String)
	if !ok {
		return errors.New("string-ref takes a string as the first argument")
	}
	idx_v, ok := stack.Pop().(Integer)
	if !ok {
		return errors.New(
			"string-ref takes an integer k as the second argument",
		)
	}
	idx_bi := big.Int(idx_v)
	idx := int(idx_bi.Int64())
	if idx < 0 || idx >= len(*s.s) {
		return errors.New("string-ref: idx out of range")
	}
	stack.Push(Char((*s.s)[idx]))
	return nil
}

func FnStringSet(nargs int) error {
	if nargs != 3 {
		return errors.New("string-set! takes 3 arguments")
	}
	s, ok := stack.Pop().(String)
	if !ok {
		return errors.New("string-set! takes a string as the first argument")
	}
	v := stack.Pop()
	idx_v, ok := v.(Integer)
	if !ok {
		return errors.New(
			"string-set! takes an integer k as the second argument",
		)
	}
	ch, ok := stack.Pop().(Char)
	if !ok {
		return errors.New("string-set! takes a char as the third argument")
	}
	idx_bi := big.Int(idx_v)
	rs := []rune(*s.s)
	idx := int(idx_bi.Int64())
	if idx < 0 || idx >= len(rs) {
		return errors.New("string-set!: idx out of range")
	}
	*s.s = string(append(append(rs[:idx], rune(ch)), rs[idx+1:]...))
	stack.Push(s)
	return nil
}

func FnStringDowncase(nargs int) error {
	if nargs != 1 {
		return errors.New("string-lowercase takes 1 argument")
	}
	s, ok := stack.Pop().(String)
	if !ok {
		return errors.New("string-lowercase takes a string as the argument")
	}
	ls := strings.ToLower(*s.s)
	stack.Push(String{&ls})
	return nil
}

func FnStringLt(nargs int) error {
	if nargs != 2 {
		return errors.New("string<? takes 2 arguments")
	}
	s1, ok := stack.Pop().(String)
	if !ok {
		return errors.New("string<? takes a string as the first argument")
	}
	s2, ok := stack.Pop().(String)
	if !ok {
		return errors.New("string<? takes a string as the second argument")
	}
	stack.Push(Boolean(*s1.s < *s2.s))
	return nil
}

func FnStringGt(nargs int) error {
	if nargs != 2 {
		return errors.New("string>? takes 2 arguments")
	}
	s1, ok := stack.Pop().(String)
	if !ok {
		return errors.New("string>? takes a string as the first argument")
	}
	s2, ok := stack.Pop().(String)
	if !ok {
		return errors.New("string>? takes a string as the second argument")
	}
	stack.Push(Boolean(*s1.s > *s2.s))
	return nil
}

func FnStringEq(nargs int) error {
	if nargs != 2 {
		return errors.New("string=? takes 2 arguments")
	}
	stack.Push(Boolean(*stack.Pop().(String).s == *stack.Pop().(String).s))
	return nil
}

func FnSubstring(nargs int) error {
	if nargs != 3 {
		return errors.New("substring takes 3 arguments")
	}
	str, ok := stack.Pop().(String)
	if !ok {
		return errors.New("substring takes a string as the first argument")
	}

	start_v := stack.Pop().(Integer)
	start_bi := big.Int(start_v)
	start := int(start_bi.Int64())

	end_v := stack.Pop().(Integer)
	end_bi := big.Int(end_v)
	end := int(end_bi.Int64())

	rs := []rune(*str.s)

	if start < 0 || end < 0 || end < start || end > len(rs) {
		return errors.New("Invalid indices for substring")
	}
	substr := string(rs[start : end])
	stack.Push(String{&substr})
	return nil
}

func FnStringAppend(nargs int) error {
	if nargs == 0 {
		return errors.New("string-append takes at least 1 argument")
	}
	s := ""
	for i := 0; i < nargs; i++ {
		str, ok := stack.Pop().(String)
		if !ok {
			return errors.New("string-append takes strings as arguments")
		}
		s = *str.s + s
	}
	stack.Push(String{&s})
	return nil
}

func FnSymbol2String(nargs int) error {
	if nargs != 1 {
		return errors.New("symbol->string takes 1 argument")
	}
	sym, ok := stack.Pop().(Symbol)
	if !ok {
		return errors.New("symbol->string takes a symbol as the argument")
	}
	s := SymbolNames[sym]
	stack.Push(String{&s})
	return nil
}

func FnNumber2String(nargs int) error {
	if nargs != 1 {
		return errors.New("number->string takes 1 argument")
	}
	v := stack.Pop()

	switch v.(type) {
	case Integer:
		n := big.Int(v.(Integer))
		s := n.String()
		stack.Push(String{&s})
	case Rational:
		n := big.Rat(v.(Rational))
		s := n.String()
		stack.Push(String{&s})
	default:
		return errors.New("number->string takes a numeric argument")
	}
	return nil
}

func FnList2String(nargs int) error {
	if nargs != 1 {
		return errors.New("string->list takes 1 argument")
	}
	l, ok := stack.Pop().(*Pair)
	if !ok {
		return errors.New("list->string takes a pair as the argument")
	}

	v, err := list2vec(l)
	if err != nil {
		return errors.New("list->string takes a proper list as the argument")
	}

	s := ""
	for i := range v {
		r, ok := v[i].(Char)
		if !ok {
			return errors.New("list->string takes a list of chars as the arg")
		}
		s += string(r)
	}
	stack.Push(String{&s})
	return nil
}

func FnStringCopy(nargs int) error {
	if nargs != 1 {
		return errors.New("string-copy takes 1 argument")
	}

	str, ok := stack.Pop().(String)
	if !ok {
		return errors.New("string-copy takes a string as the argument")
	}

	dst := *str.s
	stack.Push(String{&dst})
	return nil
}
