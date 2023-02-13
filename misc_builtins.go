package main

import (
	"errors"
	"os"
	"math/big"
	"strings"
)


func FnIsProcedure(nargs int) error {
	_, ok := stack.Pop().(*Procedure)
	stack.Push(Boolean(ok))
	return nil
}

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
			"Wrong arg count to display-internal (ports not yet implemented)",
		)
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

// SRFI 98
func FnGetEnvironmentVariables(nargs int) error {
	env_vals := []Value{}

	if nargs != 0 {
		return errors.New("get-environment-variables takes no arguments")
	}

	for _, e := range os.Environ() {
		kv := strings.SplitN(e, "=", 2)
		var k Value = String(kv[0])

		var v Value
		if len(kv) == 2 {
			v = String(kv[1])
		} else {
			v = String("")
		}
		env_vals = append(env_vals, &Pair{&k, &v})
	}
	stack.Push(vec2list(env_vals))
	return nil
}
