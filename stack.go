package main

type Stack []Value
var stack Stack

func (s *Stack) Push(v Value) {
	*s = append(*s, v)
}

func (s *Stack) Pop() Value {
	v := (*s)[len(*s) - 1]
	*s = (*s)[:len(*s) - 1]
	return v
}

func (s Stack) Top() Value {
	return s[len(s) - 1]
}

