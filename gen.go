package main

import (
	"errors"
)

func Gen(p *Procedure, v Value) error {
	switch v.(type) {
	case Boolean, String, Character, Vector, Integer, Rational:
		p.ins = append(p.ins, Ins{Imm, v, 0})
	case Symbol:
		p.ins = append(p.ins, Ins{GetVar, v, 0})
	case Pair:
		cur := v.(Pair)
		car := cur.Car
		nargs := 0
		ok := true

		for {
			cur, ok = (*cur.Cdr).(Pair)
			if !ok {
				return errors.New("Expected list, got dot pair")
			}
			if cur.Car == nil {
				break
			}
			nargs++
			Gen(p, cur.Car)
		}

		Gen(p, car)
		p.ins = append(p.ins, Ins{Call, nil, nargs})
	}
	return nil
}
