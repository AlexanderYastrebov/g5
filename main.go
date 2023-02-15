package main

import (
	"bufio"
	_ "embed"
	"fmt"
	"log"
	"os"
	"unicode"
)

//go:embed init.scm
var Init string

//go:embed srfi/case-lambda.scm
var CaseLambdaSRFI string

//go:embed srfi/lists.scm
var ListsSRFI string

func validate(code string) bool {
	nonws := 0
	count := 0
	for _, r := range []rune(code) {
		if !unicode.IsSpace(r) {
			nonws++
		}
		if r == '(' {
			count++
		} else if r == ')' {
			count--
		}
	}
	return count == 0 && nonws != 0
}

func (ctx *Procedure) Run(code string, quiet bool) {
	p := NewParser(code)
	p.skipWs()

	for len(p.data) > 0 {
		v, err := p.GetValue()
		p.skipWs()
		if err != nil {
			log.Fatalf("Error (parse): %v\n", err)
		}

		ctx.Ins = []Ins{}
		if err := ctx.Gen(v); err != nil {
			log.Fatalf("Error (gen): %v\n", err)
		}

		if err := ctx.Eval(); err != nil {
			log.Fatalf("Error (eval): %v\n", err)
		}

		if !quiet {
			fmt.Println()
			if len(stack) > 0 {
				WriteValue(stack.Top(), false)
			}
			fmt.Println()
		}
	}
}

func main() {
	Top.Scope = TopScope // Put builtins into top-level scope

	if int(SymLast) != len(SymbolNames) {
		panic("Symbol table length mismatch")
	}

	Top.Run(Init, true)
	Top.Run(CaseLambdaSRFI, true)
	Top.Run(ListsSRFI, true)
	switch len(os.Args) {
	case 1:
		reader := bufio.NewReader(os.Stdin)
		for {
			fmt.Print("> ")

			code, _ := reader.ReadString('\n')

			for !validate(code) {
				fmt.Print(">> ")
				next, _ := reader.ReadString('\n')
				code += next
			}
			Top.Run(code, false)
		}
	case 2:
		b, err := os.ReadFile(os.Args[1])
		if err != nil {
			log.Fatalln("Error: Could not read file")
		}
		Top.Run(string(b), true)
	default:
		fmt.Printf("Usage: %s [filename]", os.Args[0])
	}
}
