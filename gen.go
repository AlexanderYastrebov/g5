package main

import (
	"errors"
	"fmt"
)

func Gen(p *Procedure, v Value) error {
	switch v.(type) {
	case Boolean, String, Character, Vector, Integer, Rational:
		p.ins = append(p.ins, Ins{Imm, v, 0})
	case Symbol:
		p.ins = append(p.ins, Ins{GetVar, v, 0})
	case Pair:
		var args []Value
		cur := v.(Pair)
		for cur.Car != nil {
			var ok bool
			args = append(args, cur.Car)
			cur, ok = (*cur.Cdr).(Pair)
			if !ok {
				return errors.New("Dotted list when regular list expected")
			}
		}

		if sym, ok := args[0].(Symbol); ok {
			switch SymbolNames[sym] {
			case "set!":
				if len(args) != 3 {
					return errors.New("set! takes 2 args")
				}
				if _, ok := args[1].(Symbol); !ok {
					return errors.New("First arg to set! must be a symbol")
				}
				Gen(p, args[2])
				p.ins = append(p.ins, Ins{Set, args[1], 1})
				return nil
			case "lambda":
				lambda := Procedure{
					nil,
					[]Symbol{},
					nil,
					nil,
				}

				pair, ok := args[1].(Pair)
				if !ok {
					return errors.New(
						fmt.Sprintf("Expected argument list (%T)", args[1]))
				}
				cur := pair
				for cur.Car != nil {
					name, ok := cur.Car.(Symbol)
					if !ok {
						return errors.New("Non-symbol in argument list")
					}
					lambda.names = append(lambda.names, name)
					cur, ok = (*cur.Cdr).(Pair)
					if !ok {
						return errors.New(
							"Dotted list for args, not yet implemented")
					}
				}

				Gen(&lambda, args[2])
				p.ins = append(p.ins, Ins{Lambda, lambda, 0})
				return nil
			}
		}

		// first arg is the callee
		for i := len(args) - 1; i >= 0; i-- {
			Gen(p, args[i])
		}
		p.ins = append(p.ins, Ins{Call, nil, len(args) - 1})
	}
	return nil
}
