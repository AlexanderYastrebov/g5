package main

import (
	"testing"
	"math/big"
	"os"
)

func TestMain(m *testing.M) {
	Top.scope = TopScope // Put builtins into top-level scope
	Run(Runtime)
	os.Exit(m.Run())
}

func TestLambdas(t *testing.T) {
	Run("(define add (lambda (x y) (+ x y)))")
	result := stack.Top()
	if _, ok := result.(*Procedure); !ok {
		t.Errorf("Expected procedure, got %T", result)
	}
	
	Run("(add 2 3)")
	result, ok := stack.Top().(Integer)
	if !ok {
		t.Errorf("Expected integer, got %T", result)
	}

	if result := big.Int(result.(Integer)); result.Cmp(big.NewInt(5)) != 0 {
		t.Errorf("Expected integer, got %v", result.String())
	}
}

func TestClosuresAdder(t *testing.T) {
	Run("(define make-adder (lambda (x) (lambda (y) (+ x y))))")
	result := stack.Top()
	if _, ok := result.(*Procedure); !ok {
		t.Errorf("Expected procedure, got %T", result)
	}
	
	Run("(define add-2 (make-adder 2))")
	result = stack.Top()
	if _, ok := result.(*Procedure); !ok {
		t.Errorf("Expected procedure, got %T", result)
	}
	
	Run("(add-2 3)")
	result, ok := stack.Top().(Integer)
	if !ok {
		t.Errorf("Expected integer, got %T", result)
	}

	if result := big.Int(result.(Integer)); result.Cmp(big.NewInt(5)) != 0 {
		t.Errorf("Expected integer, got %v", result.String())
	}
}

func TestCounter(t *testing.T) {
	Run("(define (make-ctr) (set! count 0) (lambda (ctr) (+ count 1)))")
	result := stack.Top()
	if _, ok := result.(*Procedure); !ok {
		t.Errorf("Expected procedure, got %T", result)
	}
	
	Run("(define ctr (make-ctr))")
	result = stack.Top()
	if _, ok := result.(*Procedure); !ok {
		t.Errorf("Expected procedure, got %T", result)
	}
	
	Run("(ctr)")
	result, ok := stack.Top().(Integer)
	if !ok {
		t.Errorf("Expected integer, got %T", result)
	}

	if result := big.Int(result.(Integer)); result.Cmp(big.NewInt(1)) != 0 {
		t.Errorf("Expected integer, got %v", result.String())
	}
}
