package main

import "fmt"

type Stack []Value
var stack Stack

func (s *Stack) Push(v Value) {
	*s = append(*s, v)
}

func (s *Stack) Pop() Value {
	v := (*s)[len(*s) - 1]
	*s = (*s)[:len(*s) - 1]
	return v
}


type Op uint8

const (
	Imm Op = iota
	GetVar
	Call
)

type Ins struct {
	op Op
	imm Value
	nargs int
}

func (ins Ins) Print() {
	switch ins.op {
	case Imm:
		fmt.Print("IMM")
		fmt.Print("[")
		PrintValue(ins.imm)
		fmt.Println("]")
	case GetVar:
		fmt.Print("GETVAR")
		fmt.Print("[")
		PrintValue(ins.imm)
		fmt.Println("]")
	case Call:
		fmt.Print("CALL")
		fmt.Printf("(%d)\n", ins.nargs)
	}
}
