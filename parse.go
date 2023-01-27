package main

import (
	"errors"
	"fmt"
	"math/big"
	"unicode"
)

type Parser struct {
	data []rune
	line uint
}

func NewParser(code string) Parser {
	return Parser{[]rune(code), 1}
}

func (p *Parser) skipWs() {
	for len(p.data) > 0 && unicode.IsSpace(p.data[0]) {
		if p.data[0] == '\n' {
			p.line++
		}
		p.data = p.data[1:]
	}
}

func (p *Parser) GetValue() (Value, error) {
	if len(p.data) == 0 {
		return nil, errors.New(fmt.Sprintf("Line %d: Early EOF", p.line))
	}

	switch {
	case unicode.IsDigit(p.data[0]):
		digits := ""
		for len(p.data) > 0 && unicode.IsDigit(p.data[0]) {
			digits += string(p.data[0])
			p.data = p.data[1:]
		}

		if len(p.data) == 0 || p.data[0] != '.' {
			if len(p.data) != 0 &&
				!unicode.IsSpace(p.data[0]) &&
				p.data[0] != ')' {
				return nil, errors.New(fmt.Sprintf("Line %d: Invalid number",
					p.line))
			}
			var i big.Int
			i.SetString(digits, 64)
			return IntegerV(i), nil
		}

		// Decimal value
		digits += string(p.data[0])
		p.data = p.data[1:]
		for len(p.data) > 0 && unicode.IsDigit(p.data[0]) {
			digits += string(p.data[0])
			p.data = p.data[1:]
		}
		rs := []rune(digits)
		if rs[len(rs)-1] == '.' ||
			(len(p.data) != 0 &&
				!unicode.IsSpace(p.data[0]) &&
				p.data[0] != ')') {
			return nil, errors.New(fmt.Sprintf("Line %d: Invalid real (%s)",
				p.line, digits))
		}

		var r big.Rat
		r.SetString(digits)
		return RationalV(r), nil

	case p.data[0] == '"':
		p.data = p.data[1:]
		str := ""
		ended := false
		for len(p.data) > 0 && !ended {
			if p.data[0] == '\\' && len(p.data) > 1 {
				p.data = p.data[1:]
			}
			str += string(p.data[0])
			p.data = p.data[1:]
			if len(p.data) > 0 && p.data[0] == '"' {
				ended = true
			}
		}
		p.data = p.data[1:] // End quote
		if !ended {
			return nil, errors.New(fmt.Sprintf(
				"Line %d: Early EOF, non-terminated string", p.line))
		}
		return String(str), nil

	case p.data[0] == ')':
		return nil, errors.New(fmt.Sprintf("Line %d: Paren mismatch", p.line))

	case p.data[0] == '(':
		p.data = p.data[1:]
		var res Value
		var cur *Value = &res
		for p.data[0] != ')' {
			p.skipWs()
			car, err := p.GetValue()
			if err != nil {
				return nil, err
			}
			p.skipWs()
			if len(p.data) == 0 {
				return nil, errors.New(fmt.Sprintf(
					"Line %d: Early EOF (list)", p.line))
			}

			if p.data[0] == '.' {
				p.data = p.data[1:]
				p.skipWs()
				cdr, err := p.GetValue()
				if err != nil {
					return nil, err
				}
				p.skipWs()
				if len(p.data) == 0 {
					return nil, errors.New(fmt.Sprintf(
						"Line %d: Early EOF (pair)", p.line))
				}
				if p.data[0] != ')' {
					fmt.Println(p.data)
					return nil, errors.New(fmt.Sprintf(
						"Line %d: Expected closing paren (pair)", p.line))
				}

				*cur = Pair{car, &cdr}
				p.data = p.data[1:]
				return res, nil
			}

			var next Value = Pair{}
			*cur = Pair{car, &next}
			cur = &next
		}
		*cur = Nil
		p.data = p.data[1:] // Ending paren
		return res, nil

	default:
		str := ""
		for len(p.data) > 0 && !unicode.IsSpace(p.data[0]) && p.data[0] != ')' {
			str += string(p.data[0])
			p.data = p.data[1:]
		}
		SymbolNames = append(SymbolNames, str)
		return Symbol(len(SymbolNames) - 1), nil
	}
}
