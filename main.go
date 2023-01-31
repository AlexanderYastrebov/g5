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

func main() {
	Top.scope = TopScope // Put builtins into top-level scope

	reader := bufio.NewReader(os.Stdin)

	for {
		fmt.Print("> ")

		code, _ := reader.ReadString('\n')

		for !validate(code) {
			fmt.Print(">> ")
			next, _ := reader.ReadString('\n')
			code += next
		}

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

			if len(stack) > 0 {
				PrintValue(stack[len(stack) - 1])
			}
			fmt.Println()
		}
	}
}
