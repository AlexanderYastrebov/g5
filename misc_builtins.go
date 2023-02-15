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
	p.IsCont = true
	proc := stack.Pop()
	vec := []Value{}
	for i := 0; i < nargs; i++ {
		vec = append(vec)
	}
	vec = append(vec, p)

	p.StackRest = &Stack{}
	*p.StackRest = stack
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
		var k Value = String{&kv[0]}

		var v Value
		if len(kv) == 2 {
			v = String{&kv[1]}
		} else {
			s := ""
			v = String{&s}
		}
		env_vals = append(env_vals, &Pair{&k, &v})
	}
	stack.Push(vec2list(env_vals))
	return nil
}

func FnDynamicWind(nargs int) error {
	if nargs != 3 {
		return errors.New("dynamic-wind takes 3 arguments")
	}

	before, ok := stack.Pop().(*Procedure)
	if !ok {
		return errors.New("dynamic-wind takes procedure arguments")
	}
	thunk, ok := stack.Pop().(*Procedure)
	if !ok {
		return errors.New("dynamic-wind takes procedure arguments")
	}
	after, ok := stack.Pop().(*Procedure)
	if !ok {
		return errors.New("dynamic-wind takes procedure arguments")
	}

	thunk.Ins = append(
		[]Ins{
			{Imm, before, 0},
			{Call, nil, 0},
		},
		thunk.Ins...,
	)

	thunk.Ins = append(
		thunk.Ins,
		[]Ins{
			{Imm, after, 0},
			{Call, nil, 0},
		}...,
	)

	return thunk.Eval()
}

func FnValues(nargs int) error {
	stack.Push(Integer(*big.NewInt(int64(nargs))))
	return nil
}

func FnCallWithValues(nargs int) error {
	if nargs != 2 {
		return errors.New("call-with-values takes 2 arguments")
	}

	producer, ok := stack.Pop().(*Procedure)
	if !ok {
		return errors.New("call-with-values takes procedures as the args")
	}
	consumer, ok := stack.Pop().(*Procedure)
	if !ok {
		return errors.New("call-with-values takes procedures as the args")
	}

	producer.Ins = append(producer.Ins,
		[]Ins{
			{Imm, consumer, 0},
			{Call, nil, -1},
		}...,
	)

	return producer.Eval()
}
