package main

import (
	"bufio"
	"fmt"
	"os"
	"log"
)

func validate(code string) bool {
	count := 0
	for _, r := range []rune(code) {
		if r == '(' {
			count++
		} else if r == ')' {
			count--
		}
	}
	return count == 0
}

func run(code string) {
	p := NewParser(code)
	
	for len(p.data) > 0 {
		v, err := p.GetValue()
		p.skipWs()
		if err != nil {
			log.Fatalf("Erorr (parse): %s\n", err)
		}

		Top.ins = []Ins{}
		if err := Gen(Top, v); err != nil {
			log.Fatalf("Error (gen): %s\n", err)
		}

		Top.Eval()
	}
}

func main() {
	Top.scope = TopScope // Put builtins into top-level scope

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
			run(code)
		}
	case 2:
		b, err := os.ReadFile(os.Args[1])
		if err != nil {
			log.Fatalln("Error: Could not read file")
		}
		run(string(b))
	default:
		fmt.Printf("Usage: %s [filename]", os.Args[0])
	}
}
