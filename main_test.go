package main

import (
	"math/big"
	"os"
	"testing"
)

func TestMain(m *testing.M) {
	Top.Scope = TopScope // Put builtins into top-level scope
	Run(Runtime, true)
	os.Exit(m.Run())
}

func TestLambdas(t *testing.T) {
	Run("(define add (lambda (x y) (+ x y)))", true)
	result := stack.Top()
	if _, ok := result.(*Procedure); !ok {
		t.Errorf("Expected procedure, got %T", result)
	}

	Run("(add 2 3)", true)
	result, ok := stack.Top().(Integer)
	if !ok {
		t.Errorf("Expected integer, got %T", result)
	}

	if result := big.Int(result.(Integer)); result.Cmp(big.NewInt(5)) != 0 {
		t.Errorf("Expected integer, got %v", result.String())
	}
}

func TestAdder(t *testing.T) {
	Run("(define make-adder (lambda (x) (lambda (y) (+ x y))))", true)
	result := stack.Top()
	if _, ok := result.(*Procedure); !ok {
		t.Errorf("Expected procedure, got %T", result)
	}

	Run("(define add-2 (make-adder 2))", true)
	result = stack.Top()
	if _, ok := result.(*Procedure); !ok {
		t.Errorf("Expected procedure, got %T", result)
	}

	Run("(add-2 3)", true)
	result, ok := stack.Top().(Integer)
	if !ok {
		t.Errorf("Expected integer, got %T", result)
	}

	if result := big.Int(result.(Integer)); result.Cmp(big.NewInt(5)) != 0 {
		t.Errorf("Expected integer, got %v", result.String())
	}
}

func TestCounter(t *testing.T) {
	Run("(define (make-ctr) (set! count 0) (lambda (ctr) (+ count 1)))", true)
	result := stack.Top()
	if _, ok := result.(*Procedure); !ok {
		t.Errorf("Expected procedure, got %T", result)
	}

	Run("(define ctr (make-ctr))", true)
	result = stack.Top()
	if _, ok := result.(*Procedure); !ok {
		t.Errorf("Expected procedure, got %T", result)
	}

	Run("(ctr)", true)
	result, ok := stack.Top().(Integer)
	if !ok {
		t.Errorf("Expected integer, got %T", result)
	}

	if result := big.Int(result.(Integer)); result.Cmp(big.NewInt(1)) != 0 {
		t.Errorf("Expected integer, got %v", result.String())
	}
}

func TestLet(t *testing.T) {
	Run("(let ((a 0) (b 1)) b)", true)
	result, ok := stack.Top().(Integer)
	if !ok {
		t.Errorf("Expected integer, got %T", result)
	}

	if result := big.Int(result); result.Cmp(big.NewInt(1)) != 0 {
		t.Errorf("Expected integer, got %v", result.String())
	}
}

func TestAnd(t *testing.T) {
	Run("(and #t #t)", true)
	result, ok := stack.Top().(Boolean)
	if !ok {
		t.Errorf("Expected boolean, got %T", result)
	}

	if result != true {
		t.Errorf("Expected #t, got #f")
	}
}

func TestOr(t *testing.T) {
	Run("(or #t #f)", true)
	result, ok := stack.Top().(Boolean)
	if !ok {
		t.Errorf("Expected boolean, got %T", result)
	}

	if result != true {
		t.Errorf("Expected true, got false")
	}
}

func TestLetrec(t *testing.T) {
	Run("(letrec ((a #t)) #t)", true)
	result, ok := stack.Top().(Boolean)
	if !ok {
		t.Errorf("Expected boolean, got %T", result)
	}

	if result != true {
		t.Errorf("Expected true, got false")
	}
}
