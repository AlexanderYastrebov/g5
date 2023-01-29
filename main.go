package main

import (
	"bufio"
	"fmt"
	"os"
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
		v, err := p.GetValue()
		if err != nil {
			panic(err)
		}
		PrintValue(v)
		fmt.Println()

		if err := Gen(Top, v); err != nil {
			panic(err)
		}
		for _, v := range Top.ins {
			v.Print()
		}

		Top.Eval()

		if len(stack) > 0 {
			PrintValue(stack[len(stack) - 1])
		}
		fmt.Println()
	}
}
