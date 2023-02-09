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
	If
	SaveScope
)

type Ins struct {
	op    Op
	imm   Value
	nargs int
}

func (scope *Scope) Lookup(v Value) *Scope {
	sym, ok := v.(Symbol)
	if !ok {
		_, ok = v.(Scoped)
		if !ok {
			panic("Tried to lookup non-symbol")
		}
		sym = v.(Scoped).Symbol
		scope = scope.Lookup(v.(Scoped).Scope)
	}

	if scope == nil {
		return nil
	}

	if _, ok := scope.m[sym]; !ok {
		return scope.super.Lookup(sym)
	}
	return scope
}

func (p *Procedure) Eval() {
begin:
	for i, ins := range p.ins {
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
			if newp_template, ok := callee.(*Procedure); ok {
				newp := new(Procedure)
				*newp = *newp_template

				if newp.builtin != nil {
					newp.builtin(ins.nargs)
				} else {
					newp.scope = &Scope{super: newp.scope.super}
					newp.scope.m = map[Symbol]Value{}

					_, ispair := newp.args.(*Pair)
					var cur Value
					cur = newp.args

					n := ins.nargs
					for n > 0 && ispair {
						newp.scope.m[(*cur.(*Pair).Car).(Symbol)] = stack.Pop()
						n--
						cur = *cur.(*Pair).Cdr
						if _, ok := cur.(*Pair); !ok {
							break
						}
					}

					if s, ok := cur.(Symbol); ok {
						rest := new(Pair)
						cur := rest
						if n == 0 {
							newp.scope.m[s] = Empty
						} else {
							for n > 0 {
								v := stack.Pop()
								n--
								cur.Car = &v

								if n == 0 {
									cur.Cdr = &Empty
									break
								}
								var next Value = new(Pair)
								cur.Cdr = &next
								cur = next.(*Pair)
							}
							newp.scope.m[s] = rest
						}
					}

					if i == len(p.ins) - 1 { // Tail call
						p = newp
						goto begin
					}

					stack_pos := len(stack)
					newp.Eval()
					// Clear temps from stack
					top := stack.Top()
					stack = append(stack[:stack_pos], top)
				}
			} else {
				log.Fatalf("Call to non-procedure (%T)", callee)
			}
		case Lambda: // Procedure -> *Procedure
			lambda := ins.imm.(Procedure)
			lambda.scope = &Scope{}
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
			scope.m[sym] = stack.Top()
		case If:
			cond, isbool := stack.Pop().(Boolean)
			lt := stack.Pop().(Procedure)
			lf := Procedure{}

			if ins.nargs == 3 {
				lf = stack.Pop().(Procedure)
			}

			if !isbool || bool(cond) {
				lt.scope = p.scope
				lt.Eval()
			}

			if ins.nargs == 3 {
				lf.scope = p.scope
				if isbool && !bool(cond) {
					lf.Eval()
				}
			}
		case SaveScope:
			stack.Push(p.scope)
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
	case If:
		fmt.Println("IF")
	case SaveScope:
		fmt.Println("SAVE-SCOPE")
	default:
		fmt.Println("[unknown]")
	}
}
