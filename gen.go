package main

import (
	"errors"
	"fmt"
)

func list2vec(cur Pair) ([]Value, error) {
	res := []Value{}
	for cur.Car != nil {
		var ok bool
		res = append(res, cur.Car)
		cur, ok = (*cur.Cdr).(Pair)
		if !ok {
			return nil, errors.New("Dotted list when regular list expected")
		}
	}
	return res, nil
}

func Gen(p *Procedure, v Value) error {
	switch v.(type) {
	case Boolean, String, Character, Vector, Integer, Rational:
		p.ins = append(p.ins, Ins{Imm, v, 0})
	case Symbol:
		p.ins = append(p.ins, Ins{GetVar, v, 0})
	case Pair:
		var args []Value
		args, err := list2vec(v.(Pair))
		if err != nil {
			return err
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
			case "define":
				if len(args) != 3 {
					return errors.New("define takes 2 args")
				}
				if _, ok := args[1].(Symbol); !ok {
					return errors.New("First arg to define must be a symbol")
				}
				Gen(p, args[2])
				p.ins = append(p.ins, Ins{Define, args[1], 1})
				return nil
			case "lambda":
				lambda := Procedure{
					nil,
					new([]Symbol),
					nil,
					nil,
				}

				pair, ok := args[1].(Pair)
				if !ok {
					return errors.New(
						fmt.Sprintf("Expected argument list (%T)", args[1]))
				}

				var names []Value
				names, err = list2vec(pair)
				if err != nil {
					return err
				}
				
				for _, v := range names {
					*lambda.names = append(*lambda.names, v.(Symbol))
				}

				Gen(&lambda, args[2])
				p.ins = append(p.ins, Ins{Lambda, lambda, 0})
				return nil
			case "if":
				lt := Procedure{
					p.scope,
					p.names,
					[]Ins{},
					nil,
				}
				lf := lt

				Gen(&lt, args[2])
				if len(args) == 4 {
					Gen(&lf, args[3])
					p.ins = append(p.ins, Ins{Imm, lf, 0})
				}
				p.ins = append(p.ins, Ins{Imm, lt, 0})
				Gen(p, args[1])
				p.ins = append(p.ins, Ins{If, nil, len(args) - 1})
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
