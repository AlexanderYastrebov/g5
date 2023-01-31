package main

import (
	"log"
)

func FnCar(nargs int) {
	if nargs != 1 {
		log.Fatalln("Wrong arg count to car")
	}
	p := stack.Pop().(*Pair)
	stack.Push(*p.Car)
}

func FnCdr(nargs int) {
	if nargs != 1 {
		log.Fatalln("Wrong arg count to cdr")
	}
	p := stack.Pop().(*Pair)
	stack.Push(*p.Cdr)
}

func FnSetCar(nargs int) {
	if nargs != 2 {
		log.Fatalln("Wrong arg count to set-car!")
	}
	p := stack.Pop().(*Pair)
	*p.Car = stack.Pop()
}

func FnSetCdr(nargs int) {
	if nargs != 2 {
		log.Fatalln("Wrong arg count to set-cdr!")
	}
	p := stack.Pop().(*Pair)
	*p.Cdr = stack.Pop()
}
