package main

import (
	"fmt"
	"log"
)

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

func (p *Procedure) Eval() {
	for _, ins := range p.ins {
		switch ins.op {
		case Imm:
			stack.Push(ins.imm)
		case GetVar:
			cur := p.scope
			for cur != nil {
				v, ok := cur.m[ins.imm.(Symbol)]
				if ok {
					stack.Push(v)
					break
				}
			}
			if cur == nil {
				log.Fatalf("Could not find variable: %s",
					SymbolNames[ins.imm.(Symbol)])
			}
		case Call:
			callee := stack.Pop()
			if p, ok := callee.(*Procedure); ok {
				if p.builtin != nil {
					p.builtin(ins.nargs)
				}
			} else {
				log.Fatalln("Call to non-procedure")
			}
		}
	}
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
