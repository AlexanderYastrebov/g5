package main

import (
	"fmt"
	"log"
)

type Op uint8

const (
	Imm Op = iota
	GetVar
	Call
	Lambda
	Set
	Define
)

type Ins struct {
	op Op
	imm Value
	nargs int
}

func (scope *Scope) Lookup(sym Symbol) *Scope {
	if _, ok := scope.m[sym]; !ok {
		if scope.super == nil {
			return nil
		}
		return scope.super.Lookup(sym)
	}
	return scope
}

func (p *Procedure) Eval() {
begin:
	for i, ins := range p.ins {
		ins.Print()
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
				cur = cur.super
			}
			if cur == nil {
				log.Fatalf("Could not find variable: %s",
					SymbolNames[ins.imm.(Symbol)])
			}
		case Call:
			callee := stack.Pop()
			if newp, ok := callee.(*Procedure); ok {
				if newp.builtin != nil {
					newp.builtin(ins.nargs)
				} else {
					for _, name := range newp.names {
						newp.scope.m[name] = stack.Pop()
					}
					if i == len(p.ins) - 1 {
						p = newp
						goto begin
					}
					newp.Eval()
				}
			} else {
				log.Fatalf("Call to non-procedure (%T)", callee)
			}
		case Lambda: // Procedure -> *Procedure
			lambda := ins.imm.(Procedure)
			lambda.scope = new(Scope)
			lambda.scope.m = map[Symbol]Value{}
			lambda.scope.super = p.scope
			stack.Push(Value(&lambda))
		case Set, Define:
			sym := ins.imm.(Symbol)
			scope := p.scope.Lookup(sym)
			if scope == nil {
				scope = p.scope
			} else if ins.op == Define {
				fmt.Println("WARNING: Redefining variable")
			}
			scope.m[sym] = stack.Pop()
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
	case Set:
		fmt.Print("SET!")
		fmt.Print("[")
		PrintValue(ins.imm)
		fmt.Println("]")
	case Define:
		fmt.Print("DEFINE")
		fmt.Print("[")
		PrintValue(ins.imm)
		fmt.Println("]")
	case Lambda:
		fmt.Println("LAMBDA")
	default:
		fmt.Println("[unknown]")
	}
}
