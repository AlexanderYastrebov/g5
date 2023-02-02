package main

import (
	"errors"
)

func list2vec(list *Pair) ([]Value, error) {
	res := []Value{}
	for list.Car != nil {
		var ok bool
		res = append(res, *list.Car)
		list, ok = (*list.Cdr).(*Pair)
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
	case *Pair:
		var args []Value
		args, err := list2vec(v.(*Pair))
		if err != nil {
			return err
		}

		if len(args) == 0 {
			return errors.New("Empty expression")
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
				switch args[1].(type) {
				case *Pair:
					if len(args) < 2 {
						return errors.New("Function definition requires at " +
							"least one statement")
					}

					def := args[1].(*Pair)
					dest := *def.Car

					lambda := Procedure{
						args: *def.Cdr,
						ins:  []Ins{},
					}

					for _, arg := range args[2:] {
						Gen(&lambda, arg)
					}

					p.ins = append(p.ins, Ins{Lambda, lambda, 0})
					p.ins = append(p.ins, Ins{Define, dest, 1})
				case Symbol:
					if len(args) != 3 {
						return errors.New("define takes 2 args")
					}
					Gen(p, args[2])
					p.ins = append(p.ins, Ins{Define, args[1], 1})
				default:
					return errors.New("First arg to define must be a symbol")
				}
				return nil
			case "lambda":
				if len(args) < 3 {
					return errors.New("lambda requires at least one statement")
				}

				lambda := Procedure{
					args: args[1],
					ins:  []Ins{},
				}

				for _, arg := range args[2:] {
					Gen(&lambda, arg)
				}
				p.ins = append(p.ins, Ins{Lambda, lambda, 0})
				return nil
			case "if":
				lt := Procedure{args: p.args, ins: []Ins{}}
				lf := lt

				Gen(&lt, args[2])
				if len(args) > 4 {
					return errors.New("Too many args to if")
				} else if len(args) == 4 {
					Gen(&lf, args[3])
					p.ins = append(p.ins, Ins{Imm, lf, 0})
				}
				p.ins = append(p.ins, Ins{Imm, lt, 0})
				Gen(p, args[1])
				p.ins = append(p.ins, Ins{If, nil, len(args) - 1})
				return nil
			case "quote":
				if len(args) != 2 {
					return errors.New("Wrong number of args to quote")
				}
				p.ins = append(p.ins, Ins{Imm, args[1], 0})
				return nil

			// These are for the implementation of (hygenic) macros
			case "set-scope!":
				if len(args) != 2 {
					return errors.New("INTERNAL: set-scope! takes 1 arg")
				}
				lambda := Procedure{
					args: Empty,
					ins:  []Ins{},
				}
				p.ins = append(p.ins, Ins{Lambda, lambda, 0})
				p.ins = append(p.ins, Ins{Set, args[1], 1})
				return nil
			case "with-scope":
				if len(args) != 3 {
					return errors.New("Wrong number of args to with-scope")
				}
				Gen(p, args[1])
				lambda := Procedure{
					args: Empty,
					ins:  []Ins{},
				}
				Gen(&lambda, args[2])
				p.ins = append(p.ins, Ins{WithScope, lambda, 1})
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
