package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	Top.scope = TopScope // Put builtins into top-level scope

	reader := bufio.NewReader(os.Stdin)

	for {
		fmt.Print("> ")

		first, _ := reader.ReadString('\n')
		p := NewParser(first)
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
