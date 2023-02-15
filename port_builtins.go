package main

import (
	"errors"
	"os"
)

func FnIsPort(nargs int) error {
	if nargs != 1 {
		return errors.New("port? takes 1 argument")
	}

	v := stack.Pop()
	_, ok1 := v.(InputPort)
	_, ok2 := v.(OutputPort)
	stack.Push(Boolean(ok1 || ok2))
	return nil
}

func FnCallWithInputFile(nargs int) error {
	if nargs != 2 {
		return errors.New("call-with-input-file takes 2 arguments")
	}
	FnOpenInputFile(1)
	port, ok := stack.Pop().(InputPort)

	if !ok {
		return errors.New(
			"call-with-input-file takes an input port as the 1st argument",
		)
	}

	InputPortStack = append(InputPortStack, port)
	p, ok := stack.Pop().(*Procedure)
	if !ok {
		return errors.New(
			"call-with-input-file takes a procedure as the 2nd argument",
		)
	}
	p.Eval()
	port.Close()
	InputPortStack = InputPortStack[:len(InputPortStack)-1]
	return nil
}

func FnCallWithOutputFile(nargs int) error {
	if nargs != 2 {
		return errors.New("call-with-output-file takes 2 arguments")
	}
	FnOpenInputFile(1)
	port, ok:= stack.Pop().(OutputPort)

	if !ok {
		return errors.New(
			"call-with-output-file takes an output port as the 1st argument",
		)
	}

	OutputPortStack = append(OutputPortStack, port)
	p, ok := stack.Pop().(*Procedure)
	if !ok {
		return errors.New(
			"call-with-output-file takes a procedure as the 2nd argument",
		)
	}
	p.Eval()
	port.Close()
	OutputPortStack = OutputPortStack[:len(OutputPortStack)-1]
	return nil
}

func FnOpenInputFile(nargs int) error {
	if nargs != 1 {
		return errors.New("open-input-file takes 1 argument")
	}

	fname, ok := stack.Pop().(String)
	if !ok {
		return errors.New("open-input-file takes a string")
	}

	f, err := os.Open(*fname.s)
	if err != nil {
		return err
	}

	stack.Push(InputPort{f})
	return nil
}

func FnOpenOutputFile(nargs int) error {
	if nargs != 1 {
		return errors.New("open-output-file takes 1 argument")
	}

	fname, ok := stack.Pop().(String)
	if !ok {
		return errors.New("open-output-file takes a string")
	}

	f, err := os.Create(*fname.s)
	if err != nil {
		return err
	}

	stack.Push(OutputPort{f})
	return nil
}

func FnCloseInputPort(nargs int) error {
	if nargs != 1 {
		return errors.New("close-input-port takes 1 argument")
	}

	port, ok := stack.Pop().(InputPort)
	if !ok {
		return errors.New(
			"close-input-port takes an input port as the argument",
		)
	}

	port.Close()
	stack.Push(Boolean(true))
	return nil
}

func FnCloseOutputPort(nargs int) error {
	if nargs != 1 {
		return errors.New("close-output-port takes 1 argument")
	}

	port, ok := stack.Pop().(OutputPort)
	if !ok {
		return errors.New(
			"close-output-port takes an output port as the argument",
		)
	}

	port.Close()
	stack.Push(Boolean(true))
	return nil
}
