package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	reader := bufio.NewReader(os.Stdin)
	fmt.Print("> ")

	first, _ := reader.ReadString('\n')
	p := NewParser(first)
	v, err := p.GetValue()
	if err != nil {
		panic(err)
	}
	PrintValue(v)
	fmt.Println()
}
