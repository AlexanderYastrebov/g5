package main

import (
	"log"
)

func FnStringEq(nargs int) {
	if nargs != 2 {
		log.Fatalln("string=? takes 2 arguments")
	}
	stack.Push(Boolean(stack.Pop().(String) == stack.Pop().(String)))
}

func FnSymbol2String(nargs int) {
	if nargs != 1 {
		log.Fatalln("symbol->string takes 1 argument")
	}
	stack.Push(String(SymbolNames[stack.Pop().(Symbol)]))
}

func FnCharEq(nargs int) {
	if nargs != 2 {
		log.Fatalln("char=? takes 2 arguments")
	}
	stack.Push(Boolean(stack.Pop().(Character) == stack.Pop().(Character)))
}
