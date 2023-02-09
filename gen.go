package main

import (
	"errors"
	"fmt"
)

func (p *Procedure) Gen(v Value) error {
	switch v.(type) {
	case Boolean, String, Character, Vector, Integer, Rational:
		p.ins = append(p.ins, Ins{Imm, v, 0})
	case Symbol, Scoped:
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

		issym := false
		sym, ok := args[0].(Symbol)
		if ok {
			issym = true
		} else {
			if scoped, ok := args[0].(Scoped); ok {
				sym = scoped.Symbol
				issym = true
			}
		}

		if issym {
			if sr, ok := p.macros[sym]; ok {
				i := 0
				found := false
				var pattern *Pair
				f := *v.(*Pair).Cdr

				for i, pattern = range sr.Patterns {
					if IsMatch(*pattern.Cdr, f, sr.Literals) {
						found = true
						break
					}
				}

				if !found {
					return fmt.Errorf("No match found for macro %s",
						SymbolNames[sym])
				}

				m := MacroMap{}
				m.parse(*pattern.Cdr, f, sr.Literals, true)

				trans, err := m.transcribe(sr.Templates[i], false, sym)
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
				p.ins = append(p.ins, Ins{Set, args[1], 1})
				return nil
			case SymDefine:
				switch args[1].(type) {
				case *Pair:
					if len(args) < 2 {
						return errors.New("Function definition requires at " +
							"least one statement")
					}

					def := args[1].(*Pair)
					dest := *def.Car

					lambda := Procedure{
						args:   *def.Cdr,
						ins:    []Ins{},
						macros: p.macros,
					}

					for _, arg := range args[2:] {
						lambda.Gen(arg)
					}

					p.ins = append(p.ins, Ins{Lambda, lambda, 0})
					p.ins = append(p.ins, Ins{Define, dest, 1})
				case Symbol:
					if len(args) != 3 {
						return errors.New("define takes 2 args")
					}
					p.Gen(args[2])
					p.ins = append(p.ins, Ins{Define, args[1], 1})
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
					args:   args[1],
					ins:    []Ins{},
					macros: p.macros,
				}

				for _, arg := range args[2:] {
					lambda.Gen(arg)
				}
				p.ins = append(p.ins, Ins{Lambda, lambda, 0})
				return nil
			case SymIf:
				lt := Procedure{
					args:   p.args,
					ins:    []Ins{},
					macros: p.macros,
				}
				lf := lt

				lt.Gen(args[2])
				if len(args) > 4 {
					return errors.New("Too many args to if")
				} else if len(args) == 4 {
					lf.Gen(args[3])
					p.ins = append(p.ins, Ins{Imm, lf, 0})
				}
				p.ins = append(p.ins, Ins{Imm, lt, 0})
				p.Gen(args[1])
				p.ins = append(p.ins, Ins{If, nil, len(args) - 1})
				return nil
			case Quote:
				if len(args) != 2 {
					return errors.New("Wrong number of args to quote")
				}
				p.ins = append(p.ins, Ins{Imm, args[1], 0})
				return nil

			// These are for the implementation of (hygenic) macros
			case SymSaveScope:
				if len(args) != 1 {
					return errors.New("INTERNAL: save-scope takes no args")
				}
				p.ins = append(p.ins, Ins{SaveScope, nil, 0})
				return nil
			case SymDefineSyntax:
				if len(args) != 3 {
					return errors.New("Wrong number of args to define-syntax")
				}

				macroName, ok := args[1].(Symbol)
				if !ok {
					return fmt.Errorf("Expected macro name, got %T", args[1])
				}

				srl, ok := args[2].(*Pair)
				if !ok {
					return fmt.Errorf("Expected list, got %T", args[2])
				}

				srv, err := list2vec(srl)
				if err != nil {
					return err
				}

				sr, err := ParseSyntaxRules(srv)
				if err != nil {
					return err
				}

				if _, ok := p.macros[macroName]; ok {
					fmt.Printf("WARNING: Redefining macro %s",
						SymbolNames[macroName])
				}

				p.macros[macroName] = *sr
				p.ins = append(p.ins, Ins{SaveScope, nil, 0})
				p.ins = append(p.ins, Ins{Set, macroName, 1})
				return nil
			}
		}

		// first arg is the callee
		for i := len(args) - 1; i >= 0; i-- {
			p.Gen(args[i])
		}
		p.ins = append(p.ins, Ins{Call, nil, len(args) - 1})
	}
	return nil
}
