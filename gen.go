package main

import (
	"errors"
	"fmt"
)

func (p *Procedure) Gen(v Value) error {
	switch v.(type) {
	case Vector:
		panic("Vectors not yet implemented")
	case Boolean, String, Character, Integer, Rational:
		p.Ins = append(p.Ins, Ins{Imm, v, 0})
	case Symbol, Scoped:
		p.Ins = append(p.Ins, Ins{GetVar, v, 0})
	case *Pair:
		args, err := list2vec(v.(*Pair))
		if err != nil {
			return err
		}

		if len(args) == 0 {
			return errors.New("Empty expression")
		}

		car := args[0]
		if scoped, ok := args[0].(Scoped); ok {
			car = scoped.Symbol
		}

		if sym, ok := car.(Symbol); ok {
			if syntaxrules, ok := p.Macros[sym]; ok {
				var pattern *Pair
				form := *v.(*Pair).Cdr
				i, found := 0, false
				for i, pattern = range syntaxrules.Patterns {
					if IsMatch(*pattern.Cdr, form, syntaxrules.Literals) {
						found = true
						break
					}
				}

				if !found {
					return fmt.Errorf("No match found for macro %s",
						SymbolNames[sym])
				}

				m := MacroMap{}
				m.parse(*pattern.Cdr, form, syntaxrules.Literals, true)

				trans, err := m.transcribe(
					syntaxrules.Templates[i],
					false,
					sym,
				)
				if err != nil {
					return err
				}

				p.Gen(trans)
				return nil
			}

			switch sym {
			case SymSet:
				if len(args) != 3 {
					return errors.New("set! takes 2 args")
				}
				if _, ok := args[1].(Symbol); !ok {
					return errors.New("First arg to set! must be a symbol")
				}
				p.Gen(args[2])
				p.Ins = append(p.Ins, Ins{Set, args[1], 1})
				return nil
			case SymDefine:
				switch args[1].(type) {
				case *Pair:
					if len(args) < 2 {
						return errors.New("Function definition requires at " +
							"least one statement")
					}

					dest, defargs := *args[1].(*Pair).Car, *args[1].(*Pair).Cdr

					lambda := Procedure{
						Args:   Unscope(defargs),
						Ins:    []Ins{},
						Macros: p.Macros,
					}

					for _, expr := range args[2:] {
						lambda.Gen(Unscope(expr))
					}

					p.Ins = append(p.Ins, Ins{Lambda, lambda, 0})
					p.Ins = append(p.Ins, Ins{Define, dest, 1})
				case Symbol:
					if len(args) != 3 {
						return errors.New("define takes 2 args")
					}
					p.Gen(args[2])
					p.Ins = append(p.Ins, Ins{Define, args[1], 1})
				default:
					return fmt.Errorf("First arg to define must be a symbol"+
						": %T", args[1])
				}
				return nil
			case SymLambda:
				if len(args) < 3 {
					return errors.New("lambda requires at least one statement")
				}

				lambda := Procedure{
					Args:   Unscope(args[1]),
					Ins:    []Ins{},
					Macros: p.Macros,
				}

				for _, expr := range args[2:] {
					lambda.Gen(Unscope(expr))
				}
				p.Ins = append(p.Ins, Ins{Lambda, lambda, 0})
				return nil
			case SymIf:
				lt := Procedure{
					Args:   p.Args,
					Ins:    []Ins{},
					Macros: p.Macros,
				}
				lf := lt

				lt.Gen(args[2])
				if len(args) > 4 {
					return errors.New("Too many args to if")
				} else if len(args) == 4 {
					lf.Gen(args[3])
					p.Ins = append(p.Ins, Ins{Imm, lf, 0})
				}
				p.Ins = append(p.Ins, Ins{Imm, lt, 0})
				p.Gen(args[1])
				p.Ins = append(p.Ins, Ins{If, nil, len(args) - 1})
				return nil
			case Quote:
				if len(args) != 2 {
					return errors.New("Wrong number of args to quote")
				}
				p.Ins = append(p.Ins, Ins{Imm, args[1], 0})
				return nil

			// These are for the implementation of (hygenic) macros
			case SymSaveScope:
				if len(args) != 1 {
					return errors.New("Wrong number of args to save-scope")
				}
				p.Ins = append(p.Ins, Ins{SaveScope, nil, 0})
				return nil
			case SymDefineSyntax:
				if len(args) != 3 {
					return errors.New("Wrong number of args to define-syntax")
				}

				var name Symbol
				if _, ok = args[1].(Scoped); ok {
					name = args[1].(Scoped).Symbol
				} else {
					name, ok = args[1].(Symbol)
					if !ok {
						return fmt.Errorf("Expected macro name, got %t",
							args[1])
					}
				}

				_, ok := args[2].(*Pair)
				if !ok {
					return fmt.Errorf("Expected syntax-rules, got %T", args[2])
				}

				syntaxrules_v, err := list2vec(args[2].(*Pair))
				if err != nil {
					return err
				}

				syntaxrules, err := ParseSyntaxRules(syntaxrules_v)
				if err != nil {
					return err
				}

				if _, ok := p.Macros[name]; ok {
					fmt.Printf("WARNING: Redefining macro %s",
						SymbolNames[name])
				}

				p.Macros[name] = *syntaxrules
				p.Ins = append(p.Ins, Ins{SaveScope, nil, 0})
				p.Ins = append(p.Ins, Ins{Define, name, 1})
				return nil
			}
		}

		// first arg is the callee
		for i := len(args) - 1; i >= 0; i-- {
			p.Gen(args[i])
		}
		p.Ins = append(p.Ins, Ins{Call, nil, len(args) - 1})
	}
	return nil
}
