package main

import (
	"bufio"
	_ "embed"
	"fmt"
	"log"
	"os"
	"unicode"
)

//go:embed runtime.scm
var Runtime string

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

func Run(code string, quiet bool) {
	//stack = []Value{}
	p := NewParser(code)
	p.skipWs()

	for len(p.data) > 0 {
		v, err := p.GetValue()
		p.skipWs()
		if err != nil {
			log.Fatalf("Error (parse): %v\n", err)
		}

		Top.Ins = []Ins{}
		if err := Top.Gen(v); err != nil {
			log.Fatalf("Error (gen): %v\n", err)
		}

		if err := Top.Eval(); err != nil {
			log.Fatalf("Error (eval): %v\n", err)
		}

		if !quiet {
			if len(stack) > 0 {
				WriteValue(stack.Top(), false, nil)
			}
			fmt.Println()
		}
	}
}

func main() {
	Top.Scope = TopScope // Put builtins into top-level scope

	if int(Last) != len(SymbolNames) {
		log.Fatalln("Symbol table length mismatch")
	}

	Run(Runtime, true)
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
			Run(code, false)
		}
	case 2:
		b, err := os.ReadFile(os.Args[1])
		if err != nil {
			log.Fatalln("Error: Could not read file")
		}
		Run(string(b), true)
	default:
		fmt.Printf("Usage: %s [filename]", os.Args[0])
	}
}
