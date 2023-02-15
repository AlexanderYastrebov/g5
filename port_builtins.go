package main

import (
	"errors"
	"os"
	"unicode/utf8"
	"io"
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

func FnRead(nargs int) error {
	var port InputPort
	if nargs == 0 {
		port = InputPortStack[len(InputPortStack) - 1]
	} else if nargs == 1 {
		var ok bool
		port, ok = stack.Pop().(InputPort)
		if !ok {
			return errors.New("read takes an input port as the argument")
		}
	} else {
		return errors.New("Too many args to read")
	}
	
	s := ""
	for !validate(s) {
		b := make([]byte, 1)
		_, err := port.Read(b)
		if err == io.EOF {
			stack.Push(Eof{})
			return nil
		} else if err != nil {
			return err
		}
		s += string(b[0])
	}

	p := NewParser(s)
	p.skipWs()
	v, err := p.GetValue()
	if err != nil {
		return err
	}

	stack.Push(v)
	return nil
}

func FnReadChar(nargs int) error {
	var port InputPort
	if nargs == 0 {
		port = InputPortStack[len(InputPortStack) - 1]
	} else if nargs == 1 {
		var ok bool
		port, ok = stack.Pop().(InputPort)
		if !ok {
			return errors.New("read-char takes an input port as the argument")
		}
	} else {
		return errors.New("Too many args to read-char")
	}
	
	bs := []byte{}
	r := utf8.RuneError
	for r == utf8.RuneError {
		b := make([]byte, 1)
		_, err := port.Read(b)
		if err == io.EOF {
			stack.Push(Eof{})
			return nil
		} else if err != nil {
			return err
		}
		bs = append(bs, b[0])
		r, _ = utf8.DecodeRune(b)
	}

	stack.Push(Char(r))
	return nil
}

func FnPeekChar(nargs int) error {
	var port InputPort
	if nargs == 0 {
		port = InputPortStack[len(InputPortStack) - 1]
	} else if nargs == 1 {
		var ok bool
		port, ok = stack.Pop().(InputPort)
		if !ok {
			return errors.New("peek-char takes an input port as the argument")
		}
	} else {
		return errors.New("Too many args to peek-char")
	}
	
	bs := []byte{}
	r := utf8.RuneError
	for r == utf8.RuneError {
		b := make([]byte, 1)
		_, err := port.Read(b)
		if err == io.EOF {
			stack.Push(Eof{})
			return nil
		} else if err != nil {
			return err
		}
		bs = append(bs, b[0])
		r, _ = utf8.DecodeRune(b)
	}
	port.Seek(-int64(len(bs)), io.SeekCurrent)

	stack.Push(Char(r))
	return nil
}

func FnIsEofObject(nargs int) error {
	if nargs != 1 {
		return errors.New("eof-object? takes 1 argument")
	}

	_, ok := stack.Pop().(Eof)
	stack.Push(Boolean(ok))
	return nil
}

func FnIsCharReady(nargs int) error {
	var port InputPort
	if nargs == 0 {
		port = InputPortStack[len(InputPortStack) - 1]
	} else if nargs == 1 {
		var ok bool
		port, ok = stack.Pop().(InputPort)
		if !ok {
			return errors.New("char-ready? takes an input port as the argument")
		}
	} else {
		return errors.New("Too many args to char-ready?")
	}

	if _, err := port.Read(make([]byte, 1)); err == io.EOF {
		stat, _ := port.Stat()
		stack.Push(Boolean((stat.Mode() & os.ModeCharDevice) != 0))
		return nil
	} else if err != nil {
		return err
	}
	port.Seek(-1, io.SeekCurrent)
	stack.Push(Boolean(true))
	return nil
}

func FnWrite(nargs int) error {
	var port OutputPort
	v := stack.Pop()
	if nargs == 1 {
		port = OutputPortStack[len(OutputPortStack) - 1]
	} else if nargs == 2 {
		var ok bool
		port, ok = stack.Pop().(OutputPort)
		if !ok {
			return errors.New("write takes an input port as the argument")
		}
	} else {
		return errors.New("Too many args to write")
	}

	OutputPortStack = append(OutputPortStack, port)
	WriteValue(v, false)
	OutputPortStack = OutputPortStack[:len(OutputPortStack) - 1]
	stack.Push(v)
	return nil
}

func FnDisplay(nargs int) error {
	var port OutputPort
	v := stack.Pop()
	if nargs == 1 {
		port = OutputPortStack[len(OutputPortStack) - 1]
	} else if nargs == 2 {
		var ok bool
		port, ok = stack.Pop().(OutputPort)
		if !ok {
			return errors.New("display takes an input port as the argument")
		}
	} else {
		return errors.New("Too many args to display")
	}

	OutputPortStack = append(OutputPortStack, port)
	WriteValue(v, true)
	OutputPortStack = OutputPortStack[:len(OutputPortStack) - 1]
	stack.Push(v)
	return nil
}
