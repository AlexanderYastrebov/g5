package main

import (
	"errors"
	"fmt"
	"math/big"
	"unicode"
	"strings"
)

var delim = map[rune]bool{
	';': true,
	')': true,
	' ': true,
	'\n': true,
	'\r': true,
	'\t': true,
}

type Parser struct {
	data []rune
	line uint
}

func NewParser(code string) Parser {
	return Parser{[]rune(code), 1}
}

func (p *Parser) skipWs() {
	for len(p.data) > 0 && (unicode.IsSpace(p.data[0]) || p.data[0] == ';') {
		if p.data[0] == ';' {
			for p.data[0] != '\n' {
				p.data = p.data[1:]
			}
		}
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
			i.SetString(digits, 10)
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
		if rs[len(rs)-1] == '.' || (len(p.data) != 0 && !delim[p.data[0]]) {
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
		*cur = Empty
		p.data = p.data[1:] // Ending paren
		return res, nil
	
	case p.data[0] == '#':
		p.data = p.data[1:]
		if len(p.data) == 0 {
			return nil, errors.New(fmt.Sprintf(
				"Line %d: Early EOF (#)", p.line))
		}

		if ch := p.data[0]; ch == 't' || ch == 'f' {
			p.data = p.data[1:]
			if len(p.data) > 0 && !delim[p.data[0]] {
				return nil, errors.New(fmt.Sprintf(
					"Line %d: Invalid # sequence", p.line))
			}
			return Boolean(ch == 't'), nil
		} else if ch == '\\' {
			p.data = p.data[1:]
			if len(p.data) == 0 {
				return nil, errors.New(fmt.Sprintf(
					"Line %d: Early EOF (character)", p.line))
			}

			if len(p.data) == 1 || delim[p.data[1]] {
				ch, p.data = p.data[0], p.data[1:]
				return Character(ch), nil
			}

			for _, v := range []string{"space", "newline"} {
				slen := len(v)
				if len(p.data) >= slen - 1 {
					if len(p.data) == slen || delim[p.data[slen]] {
						str := string(p.data[:slen])
						p.data = p.data[slen:]
						if strings.ToLower(str) == "space" {
							return Character(' '), nil
						} else if strings.ToLower(str) == "newline" {
							return Character('\n'), nil
						}
					}
				}
			}
			return nil, errors.New(fmt.Sprintf(
				"Line %d: Invalid # sequence", p.line))
		} else if ch == '(' {
			p.data = p.data[1:]

			vec := Vector{}
			for p.data[0] != ')' {
				p.skipWs()
				new, err := p.GetValue()
				if err != nil {
					return nil, err
				}
				vec = append(vec, new)
				p.skipWs()

				if len(p.data) == 0 {
					return nil, errors.New(fmt.Sprintf(
						"Line %d: Early EOF (vector)", p.line))
				}
			}
			return vec, nil
		} else {
			return nil, errors.New(fmt.Sprintf(
				"Line %d: Invalid # sequence", p.line))
		}
	
	case p.data[0] == '\''|| p.data[0] == '`' || p.data[0] == ',':
		str := string(p.data[0])

		p.data = p.data[1:]
		if len(p.data) != 0 && p.data[0] == '@' {
			str += "@"
			p.data = p.data[1:]
		}
		val, err := p.GetValue()
		if err != nil {
			return nil, err
		}

		var tail Value = Pair{val, &Empty}
		switch str {
		case "\\":
			return Pair{Quote, &tail}, nil
		case ",":
			return Pair{Unquote, &tail}, nil
		case "`":
			return Pair{Quasiquote, &tail}, nil
		case ",@":
			return Pair{UnquoteSplicing, &tail}, nil
		default:
			panic("Unreachable")
		}

	default: // Symbol
		str := ""
		for len(p.data) > 0 && !delim[p.data[0]] {
			str += string(p.data[0])
			p.data = p.data[1:]
		}

		str = strings.ToLower(str) // Symbols are case-insensitive

		for i, v := range SymbolNames {
			if v == str {
				return Symbol(i), nil
			}
		}

		SymbolNames = append(SymbolNames, str)
		return Symbol(len(SymbolNames) - 1), nil
	}
}
