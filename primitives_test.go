// Copyright 2017, Timothy Bogdala <tdb@animal-machine.com>
// See the LICENSE file for more details.

package gimmick

import (
	"math"
	"testing"
)

func cmpLists(l1, l2 List) bool {
	if len(l1) != len(l2) {
		return false
	}
	for i, sexp1 := range l1 {
		// checkt to see if l1[i] is a list and l2[i] is a list
		sexp1List, isSexp1List := sexp1.(List)
		if isSexp1List {
			sexp2List, isSexp2List := l2[i].(List)
			if !isSexp2List {
				// one of these isn't a list so return false
				return false
			}
			// do a recursive check into this list
			if !cmpLists(sexp1List, sexp2List) {
				return false
			}
		} else if sexp1 != l2[i] {
			// do a basic comparison on the remaining types
			return false
		}
	}
	return true
}

func doTestEvalForList(t *testing.T, baseEnv *Environment, expected List, code string) {
	// copy environment to avoid polluting original Environment
	env := baseEnv.Copy()

	sexp, err := ParseString(code)
	if err != nil {
		t.Errorf("Unable to parse code: %v\nCode:\n%s\n", err, code)
	}

	result := Eval(sexp, env)
	listVal, found := result.(List)

	if !found || len(listVal) != len(expected) || !cmpLists(listVal, expected) {
		t.Errorf("Failed to evaluate to the correct value for \"%s\". Got: %v and expected: %v",
			code, result, expected)
	}
}

func doTestEvalForBool(t *testing.T, baseEnv *Environment, expected Bool, code string) {
	// copy environment to avoid polluting original Environment
	env := baseEnv.Copy()

	sexp, err := ParseString(code)
	if err != nil {
		t.Errorf("Unable to parse code: %v\nCode:\n%s\n", err, code)
	}

	result := Eval(sexp, env)
	boolVal, found := result.(Bool)
	if !found || boolVal != expected {
		t.Errorf("Failed to evaluate to the correct value for \"%s\". Got: %v", code, result)
	}
}

func doTestEvalForInt(t *testing.T, baseEnv *Environment, expected Integer, code string) {
	// copy environment to avoid polluting original Environment
	env := baseEnv.Copy()

	sexp, err := ParseString(code)
	if err != nil {
		t.Errorf("Unable to parse code: %v\nCode:\n%s\n", err, code)
	}

	result := Eval(sexp, env)
	intVal, found := result.(Integer)
	if !found || intVal != expected {
		t.Errorf("Failed to evaluate to the correct value for \"%s\". Got: %v", code, result)
	}
}

func doTestEvalForFloat(t *testing.T, baseEnv *Environment, expected Float, code string) {
	const epsillon = 0.0001

	// copy environment to avoid polluting original Environment
	env := baseEnv.Copy()

	sexp, err := ParseString(code)
	if err != nil {
		t.Errorf("Unable to parse code: %v\nCode:\n%s\n", err, code)
	}

	result := Eval(sexp, env)
	floatVal, found := result.(Float)
	if !found || math.Abs(float64(floatVal-expected)) > epsillon {
		t.Errorf("Failed to evaluate to the correct value for \"%s\". Got: %v", code, result)
	}
}

func TestBasicMath(t *testing.T) {
	// setup the test environment
	env := NewEnvironment(nil)
	env.SetupPrimitives()

	doTestEvalForInt(t, env, 10,
		`(begin
           (define r 10)
           r)`)

	// ========================================================================
	// addition tests
	doTestEvalForInt(t, env, 15,
		`(begin
           (define r 10)
           (+ r 5))`)
	doTestEvalForInt(t, env, 30,
		`(begin
           (define r 10)
           (+ r r r))`) // three of same symbols
	doTestEvalForInt(t, env, 15,
		`(begin
           (define r 10)
           (define x 5)
           (+ r x))`) // two symbols
	doTestEvalForInt(t, env, 15, "(+ 10 5)")         // basic additions
	doTestEvalForInt(t, env, 35, "(+ 10 5 20)")      // more than two parameters
	doTestEvalForInt(t, env, 35, "(+ 35)")           // single parameter
	doTestEvalForInt(t, env, 0, "(+)")               // no parameters
	doTestEvalForInt(t, env, 16, "(+ 5 11.2)")       // float to int
	doTestEvalForInt(t, env, 5, "(+ -30 35)")        // negative numbers
	doTestEvalForInt(t, env, 25, "(+ (+ 10 10) 5)")  // nested +
	doTestEvalForInt(t, env, 20, "(+ (+ 10 10))")    // nested single +
	doTestEvalForInt(t, env, 20, "(+ (+ 10 10) ())") // non-number

	doTestEvalForFloat(t, env, 15.0, "(+ 10.0 5.0)")          // basic additions
	doTestEvalForFloat(t, env, 35.0, "(+ 10.0 5 20)")         // more than two parameters
	doTestEvalForFloat(t, env, 35.0, "(+ 35.0)")              // single parameter
	doTestEvalForFloat(t, env, 16.0, "(+ 5.0 11)")            // float to int
	doTestEvalForFloat(t, env, 5.0, "(+ -30.0 35.0)")         // negative numbers
	doTestEvalForFloat(t, env, 25.0, "(+ (+ 10.0 10.0) 5.0)") // nested +
	doTestEvalForFloat(t, env, 20.0, "(+ (+ 10.0 10.0))")     // nested single +
	doTestEvalForFloat(t, env, 20.0, "(+ (+ 10.0 10.0) ())")  // non-number

	// ========================================================================
	// subtraction tests
	doTestEvalForInt(t, env, 5,
		`(begin
           (define r 10)
           (- r 5))`)
	doTestEvalForInt(t, env, -10,
		`(begin
           (define r 10)
           (- r r r))`) // three of same symbols
	doTestEvalForInt(t, env, 5,
		`(begin
           (define r 10)
           (define x 5)
           (- r x))`) // two symbols
	doTestEvalForInt(t, env, 5, "(- 10 5)")         // basic subtractions
	doTestEvalForInt(t, env, -15, "(- 10 5 20)")    // more than two parameters
	doTestEvalForInt(t, env, 35, "(- 35)")          // single parameter
	doTestEvalForInt(t, env, 0, "(-)")              // no parameters
	doTestEvalForInt(t, env, -6, "(- 5 11.2)")      // float to int
	doTestEvalForInt(t, env, -65, "(- -30 35)")     // negative numbers
	doTestEvalForInt(t, env, -5, "(- (- 10 10) 5)") // nested -
	doTestEvalForInt(t, env, 2, "(- (- 12 10))")    // nested single -
	doTestEvalForInt(t, env, 5, "(- (- 15 10) ())") // non-number

	doTestEvalForFloat(t, env, 5.0, "(- 10.0 5.0)")           // basic subtractions
	doTestEvalForFloat(t, env, -15.0, "(- 10.0 5 20)")        // more than two parameters
	doTestEvalForFloat(t, env, 35.0, "(- 35.0)")              // single parameter
	doTestEvalForFloat(t, env, -6.0, "(- 5.0 11)")            // float to int
	doTestEvalForFloat(t, env, -65.0, "(- -30.0 35.0)")       // negative numbers
	doTestEvalForFloat(t, env, -3.0, "(- (- 12.0 10.0) 5.0)") // nested -
	doTestEvalForFloat(t, env, 2.0, "(- (- 12.0 10.0))")      // nested single -
	doTestEvalForFloat(t, env, 2.0, "(- (- 12.0 10.0) ())")   // non-number

	// ========================================================================
	// multiplication tests
	doTestEvalForInt(t, env, 50,
		`(begin
           (define r 10)
           (* r 5))`)
	doTestEvalForInt(t, env, 1000,
		`(begin
           (define r 10)
           (* r r r))`) // three of same symbols
	doTestEvalForInt(t, env, 50,
		`(begin
           (define r 10)
           (define x 5)
           (* r x))`) // two symbols
	doTestEvalForInt(t, env, 50, "(* 10 5)")          // basic subtractions
	doTestEvalForInt(t, env, 100, "(* 10 5 2)")       // more than two parameters
	doTestEvalForInt(t, env, 35, "(* 35)")            // single parameter
	doTestEvalForInt(t, env, 0, "(*)")                // no parameters
	doTestEvalForInt(t, env, 55, "(* 5 11.2)")        // float to int
	doTestEvalForInt(t, env, -105, "(* -3 35)")       // negative numbers
	doTestEvalForInt(t, env, 500, "(* (* 10 10) 5)")  // nested -
	doTestEvalForInt(t, env, 120, "(* (* 12 10))")    // nested single -
	doTestEvalForInt(t, env, 150, "(* (* 15 10) ())") // non-number

	doTestEvalForFloat(t, env, 50.0, "(* 10.0 5.0)")           // basic subtractions
	doTestEvalForFloat(t, env, 100.0, "(* 10.0 5 2)")          // more than two parameters
	doTestEvalForFloat(t, env, 35.0, "(* 35.0)")               // single parameter
	doTestEvalForFloat(t, env, 55.0, "(* 5.0 11)")             // float to int
	doTestEvalForFloat(t, env, -105.0, "(* -3.0 35.0)")        // negative numbers
	doTestEvalForFloat(t, env, 600.0, "(* (* 12.0 10.0) 5.0)") // nested -
	doTestEvalForFloat(t, env, 120.0, "(* (* 12.0 10.0))")     // nested single -
	doTestEvalForFloat(t, env, 120.0, "(* (* 12.0 10.0) ())")  // non-number

	// ========================================================================
	// division tests
	doTestEvalForInt(t, env, 2,
		`(begin
           (define r 10)
           (/ r 5))`)
	doTestEvalForFloat(t, env, 0.1,
		`(begin
           (define r 10.0)
           (/ r r r))`) // three of same symbols
	doTestEvalForInt(t, env, 2,
		`(begin
           (define r 10)
           (define x 5)
           (/ r x))`) // two symbols
	doTestEvalForInt(t, env, 2, "(/ 10 5)")         // basic subtractions
	doTestEvalForInt(t, env, 1, "(/ 10 5 2)")       // more than two parameters
	doTestEvalForInt(t, env, 35, "(/ 35)")          // single parameter
	doTestEvalForInt(t, env, 0, "(/)")              // no parameters
	doTestEvalForInt(t, env, 2, "(/ 22 11.2)")      // float to int
	doTestEvalForInt(t, env, -10, "(/ -30 3)")      // negative numbers
	doTestEvalForInt(t, env, 2, "(/ (/ 100 10) 5)") // nested -
	doTestEvalForInt(t, env, 6, "(/ (/ 12 2))")     // nested single -
	doTestEvalForInt(t, env, 7, "(/ (/ 15 2))")     // nested single -
	doTestEvalForInt(t, env, 6, "(/ (/ 12 2) ())")  // non-number

	doTestEvalForFloat(t, env, 2.0, "(/ 10.0 5.0)")         // basic subtractions
	doTestEvalForFloat(t, env, 1.0, "(/ 10.0 5 2)")         // more than two parameters
	doTestEvalForFloat(t, env, 35.0, "(/ 35.0)")            // single parameter
	doTestEvalForFloat(t, env, 2.0, "(/ 22.0 11)")          // float to int
	doTestEvalForFloat(t, env, -1.0, "(/ -3.0 3.0)")        // negative numbers
	doTestEvalForFloat(t, env, 2.0, "(/ (/ 20.0 2.0) 5.0)") // nested -
	doTestEvalForFloat(t, env, 6.0, "(/ (/ 12.0 2.0))")     // nested single -
	doTestEvalForFloat(t, env, 7.5, "(/ (/ 15.0 2.0))")     // nested single -
	doTestEvalForFloat(t, env, 6.0, "(/ (/ 12.0 2.0) ())")  // non-number

}

func TestBasicComparisons(t *testing.T) {
	// setup the test environment
	env := NewEnvironment(nil)
	env.SetupPrimitives()

	// greater
	doTestEvalForBool(t, env, true, "(>)")
	doTestEvalForBool(t, env, true, "(> 10)")
	doTestEvalForBool(t, env, false, "(> ghosts)")
	doTestEvalForBool(t, env, false, "(> 10 5 ghosts)")
	doTestEvalForBool(t, env, true, "(> 10 5)")
	doTestEvalForBool(t, env, true, "(> 10 5.0)")
	doTestEvalForBool(t, env, true, "(> 10.0 5.0)")
	doTestEvalForBool(t, env, true, "(> 10.0 5)")
	doTestEvalForBool(t, env, true, "(> 10 5 4 3 2 1)")
	doTestEvalForBool(t, env, false, "(> 10 50)")
	doTestEvalForBool(t, env, false, "(> 10 2 50)")
	doTestEvalForBool(t, env, false, "(> 10 50.0)")
	doTestEvalForBool(t, env, false, "(> 10.0 50)")
	doTestEvalForBool(t, env, false, "(> 10.0 50.0)")

	// greatereq
	doTestEvalForBool(t, env, true, "(>=)")
	doTestEvalForBool(t, env, true, "(>= 10)")
	doTestEvalForBool(t, env, false, "(>= ghosts)")
	doTestEvalForBool(t, env, false, "(>= 10 5 ghosts)")
	doTestEvalForBool(t, env, true, "(>= 10 5)")
	doTestEvalForBool(t, env, true, "(>= 10 5.0)")
	doTestEvalForBool(t, env, true, "(>= 10.0 5.0)")
	doTestEvalForBool(t, env, true, "(>= 10.0 5)")
	doTestEvalForBool(t, env, true, "(>= 10.0 10)")
	doTestEvalForBool(t, env, true, "(>= 10 10)")
	doTestEvalForBool(t, env, true, "(>= 10 10 3 2 1)")
	doTestEvalForBool(t, env, true, "(>= 10 5 4 3 2 1)")
	doTestEvalForBool(t, env, false, "(>= 10 50)")
	doTestEvalForBool(t, env, false, "(>= 10 2 50)")
	doTestEvalForBool(t, env, false, "(>= 10 50.0)")
	doTestEvalForBool(t, env, false, "(>= 10.0 50)")
	doTestEvalForBool(t, env, false, "(>= 10.0 50.0)")

	// lesser
	doTestEvalForBool(t, env, true, "(<)")
	doTestEvalForBool(t, env, true, "(< 10)")
	doTestEvalForBool(t, env, false, "(< ghosts)")
	doTestEvalForBool(t, env, false, "(< 5 10 ghosts)")
	doTestEvalForBool(t, env, false, "(< 10 5)")
	doTestEvalForBool(t, env, false, "(< 10 5.0)")
	doTestEvalForBool(t, env, false, "(< 10.0 5.0)")
	doTestEvalForBool(t, env, false, "(< 10.0 5)")
	doTestEvalForBool(t, env, false, "(< 10 5 4 3 2 1)")
	doTestEvalForBool(t, env, true, "(< 10 50 60 70)")
	doTestEvalForBool(t, env, true, "(< 10 50)")
	doTestEvalForBool(t, env, true, "(< 10 20 50)")
	doTestEvalForBool(t, env, true, "(< 10 50.0)")
	doTestEvalForBool(t, env, true, "(< 10.0 50)")
	doTestEvalForBool(t, env, true, "(< 10.0 50.0)")

	// lessereq
	doTestEvalForBool(t, env, true, "(<=)")
	doTestEvalForBool(t, env, true, "(<= 10)")
	doTestEvalForBool(t, env, false, "(<= ghosts)")
	doTestEvalForBool(t, env, false, "(<= 5 10 ghosts)")
	doTestEvalForBool(t, env, false, "(<= 10 5)")
	doTestEvalForBool(t, env, false, "(<= 10 5.0)")
	doTestEvalForBool(t, env, false, "(<= 10.0 5.0)")
	doTestEvalForBool(t, env, false, "(<= 10.0 5)")
	doTestEvalForBool(t, env, false, "(<= 10 5 4 3 2 1)")
	doTestEvalForBool(t, env, true, "(<= 10 50 60 70)")
	doTestEvalForBool(t, env, true, "(<= 10 50)")
	doTestEvalForBool(t, env, true, "(<= 10 20 50)")
	doTestEvalForBool(t, env, true, "(<= 10 50.0)")
	doTestEvalForBool(t, env, true, "(<= 10.0 50)")
	doTestEvalForBool(t, env, true, "(<= 10.0 50.0)")
	doTestEvalForBool(t, env, true, "(<= 10.0 10)")
	doTestEvalForBool(t, env, true, "(<= 10 10)")
	doTestEvalForBool(t, env, true, "(<= 10 10 11 12 13 13 14)")

	// eqv
	doTestEvalForBool(t, env, true, "(eqv?)")
	doTestEvalForBool(t, env, true, "(eqv? 10)")
	doTestEvalForBool(t, env, true, "(eqv? ghosts)")
	doTestEvalForBool(t, env, false, "(eqv? 10 ghosts)")
	doTestEvalForBool(t, env, false, "(eqv? 10 5)")
	doTestEvalForBool(t, env, true, "(eqv? 10 10)")
	doTestEvalForBool(t, env, true, "(eqv? 10.2 10.2)")
	doTestEvalForBool(t, env, true,
		`(begin
			(define x 100)
			(define y 100)
			(eqv? x y))`)

	doTestEvalForBool(t, env, true, "(eqv? (list 1 2) (list 1 2))")
}

func TestBasicListOps(t *testing.T) {
	// setup the test environment
	env := NewEnvironment(nil)
	env.SetupPrimitives()

	doTestEvalForList(t, env, List{Integer(1), Integer(2), Integer(3)}, "(list 1 2 3)")
	doTestEvalForList(t, env, List{Integer(1), Integer(2), Float(3.14)}, "(list 1 2 3.14)")
	doTestEvalForList(t, env, List{Integer(1), Integer(2), Integer(3), List{Float(3.1), Float(3.2)}},
		"(list 1 2 3 (list 3.1 3.2))")
	doTestEvalForList(t, env, List{Integer(1), List{Float(3.1), Float(3.2)}, Integer(2), List{Integer(3)}},
		"(list 1 (list 3.1 3.2) 2 (list 3))")

	doTestEvalForInt(t, env, 1, "(car (list 1 2 3))")
	doTestEvalForInt(t, env, 4, "(car (list 4 2 3) 2)")
	doTestEvalForList(t, env, List{Integer(2), Integer(3)}, "(cdr (list 1 2 3))")
}

func TestBasicConditionals(t *testing.T) {
	// setup the test environment
	env := NewEnvironment(nil)
	env.SetupPrimitives()

	doTestEvalForList(t, env, List{}, "(if)")
	doTestEvalForList(t, env, List{}, "(if true)")
	doTestEvalForList(t, env, List{}, "(if false true)")
	doTestEvalForBool(t, env, true, "(if true true)")
	doTestEvalForBool(t, env, true, "(if true true false)")
	doTestEvalForBool(t, env, false, "(if false true false)")
	doTestEvalForBool(t, env, false, "(if true false true)")
	doTestEvalForBool(t, env, true, "(if false false true)")

	doTestEvalForBool(t, env, false, "(if (eqv? 10 10) false true)")
	doTestEvalForBool(t, env, false, "(if (eqv? 10 10) (eqv? 10 11.1) (eqv? 0.0 0.0))")
}

func TestBasicQuote(t *testing.T) {
	// setup the test environment
	env := NewEnvironment(nil)
	env.SetupPrimitives()

	doTestEvalForList(t, env, List{Symbol("+"), Integer(5), Integer(5)}, "(quote (+ 5 5))")
	doTestEvalForBool(t, env, true, "(quote true)")
	doTestEvalForBool(t, env, false, "(quote false)")
	doTestEvalForInt(t, env, 1, "(quote 1)")
	doTestEvalForFloat(t, env, 1.1, "(quote 1.1)")
}

func TestBasicQuasiquote(t *testing.T) {
	// setup the test environment
	env := NewEnvironment(nil)
	env.SetupPrimitives()

	doTestEvalForList(t, env, List{Integer(5), Integer(5), Integer(5)}, "(quasiquote (5 5 5))")
	doTestEvalForList(t, env, List{Integer(5), Integer(5), Integer(5)}, "`(5 5 5)")
	doTestEvalForList(t, env, List{Integer(5)}, "(quasiquote ((unquote (+ 2 3))))")
	doTestEvalForList(t, env, List{Integer(5)}, "`(,(+ 2 3))")
	doTestEvalForList(t, env, List{Integer(5), Integer(5)}, "(quasiquote (5 (unquote (+ 2 3))))")
	doTestEvalForList(t, env, List{Integer(5), Integer(5)}, "``(5 ,(+ 2 3))")
	doTestEvalForList(t, env, List{Integer(1), Integer(2), Integer(3), Integer(4), Integer(5)},
		"(quasiquote (1 (unquote-splicing (list 2 3)) 4 5))")
	doTestEvalForList(t, env, List{Integer(1), Integer(2), Integer(3), Integer(4), Integer(5)},
		"`(1 ,@(list 2 3) 4 5)")
	doTestEvalForList(t, env, List{Integer(1), Integer(2), Integer(3), Integer(4), Integer(5)},
		"`(1 ,@(list 2 3) ,(+ 2 2) 5)")
	doTestEvalForList(t, env, List{Integer(5), List{Symbol("list"), Integer(5)}},
		"(quasiquote (5 (list (unquote (+ 2 3)))))")
	doTestEvalForList(t, env, List{Integer(5), List{Symbol("list"), Integer(5)}},
		"`(5 (list ,(+ 2 3)))")
	doTestEvalForList(t, env, List{Integer(5), List{Integer(5)}},
		"(quasiquote (5 (quasiquote ((unquote (+ 2 3))))))")
	doTestEvalForList(t, env, List{Integer(5), List{Integer(5)}},
		"`(5 `(,(+ 2 3)))")
	doTestEvalForList(t, env, List{Symbol("firstname"), String("JOHN")},
		"(begin (define name \"JOHN\") `(firstname ,name))")

	// edge cases
	doTestEvalForList(t, env, List{}, "(quasiquote)")
	doTestEvalForList(t, env, List{}, "(quasiquote 5)") // needs a list
	doTestEvalForList(t, env, List{Integer(5)}, "(quasiquote (5 (unquote)))")
	doTestEvalForList(t, env, List{Integer(5)}, "(quasiquote (5 (unquote-splicing)))")
}

func TestBasicLambdaForms(t *testing.T) {
	// setup the test environment
	baseEnv := NewEnvironment(nil)
	baseEnv.SetupPrimitives()

	// copy environment to avoid polluting original Environment
	env := baseEnv.Copy()

	code := "(lambda (x) (+ x x))"
	sexp, err := ParseString(code)
	if err != nil {
		t.Errorf("Unable to parse code: %v\nCode:\n%s\n", err, code)
	}

	result := Eval(sexp, env)
	procVal, found := result.(Procedure)

	if !found || procVal.ParentEnv != env || len(procVal.Args) != 1 || len(procVal.Body.(List)) != 3 {
		t.Errorf("Failed to evaluate to the correct value for \"%s\". Got: %v", code, procVal)
	}

	// basic function tests
	doTestEvalForInt(t, env, 10, `(begin 
		(define foo (lambda (x) (+ x x))) 
		(foo 5))`)

	// test shadowing
	doTestEvalForInt(t, env, 10, `(begin 
		(define x 50)
		(define foo (lambda (x) (+ x x))) 
		(foo 5))`)

	// test accessing higher environments
	doTestEvalForInt(t, env, 55, `(begin 
		(define y 50)
		(define foo (lambda (x) (+ x y))) 
		(foo 5))`)

	// test more params
	doTestEvalForInt(t, env, 35, `(begin 
		(define foo (lambda (x y z) (+ x y z))) 
		(foo 5 10 20))`)

	// test lambda forms as the first expression in a list
	doTestEvalForInt(t, env, 35, `((lambda (x) (+ x 25)) 10)`)
	doTestEvalForInt(t, env, 35, `(begin ((lambda (x y) (+ x y)) 10 25))`)

	// test calling a function that doesn't exist
	doTestEvalForList(t, env, List{}, "(x3 500 500 500.1)")

	// test variable number of parameters
	doTestEvalForList(t, env, List{Integer(1), Integer(2), Integer(3), Integer(4), Integer(5)}, `(begin 
		(define foo (lambda (x y . z)
			(quasiquote (,x ,y ,@z))))
		(foo 1 2 3 4 5)))
		`)
	doTestEvalForList(t, env, List{Integer(1), Integer(2), Integer(3)}, `(begin 
		(define foo (lambda (x y . z)
			(quasiquote (,x ,y ,@z))))
		(foo 1 2 3)))
		`)
	doTestEvalForList(t, env, List{Integer(1), Integer(2)}, `(begin 
		(define foo (lambda (x y . z)
			(quasiquote (,x ,y ,@z))))
		(foo 1 2)))
		`)
	doTestEvalForList(t, env, List{Integer(1), Integer(2), Integer(3), Integer(4), Integer(5), String("Peanut")}, `(begin 
		(define foo (lambda (x y . z)
			(quasiquote (,x ,y ,@z))))
		(foo 1 2 3 4 5 "Peanut")))
		`)
	doTestEvalForList(t, env, List{Integer(1), Integer(2), List{Integer(3), Integer(4), Integer(5)}}, `(begin 
		(define foo (lambda (x y . z)
			(quasiquote (,x ,y ,z))))
		(foo 1 2 3 4 5)))
		`)

	doTestEvalForInt(t, env, 6, `(let ((x 2) (y 3)) (* x y))`)
	doTestEvalForInt(t, env, 35, `
	(let ((x 2) (y 3))
	(let ((x 7)
		  (z (+ x y)))
	  (* z x)))`)

	doTestEvalForInt(t, env, 70, `
	(let ((x 2) (y 3))
	  (let* ((x 7)
			 (z (+ x y)))
		(* z x)))`)

	doTestEvalForBool(t, env, true, `
	(letrec ((zero? (lambda (x) (eqv? x 0)))
		     (even? (lambda (n) (if (zero? n) true (odd? (- n 1)))))
		     (odd?  (lambda (n) (if (zero? n) false (even? (- n 1))))))
	(even? 88))`)
}

func TestBasicTimedApply(t *testing.T) {
	// setup the test environment
	baseEnv := NewEnvironment(nil)
	baseEnv.SetupPrimitives()

	// timed-apply with a lambda and 1 parameter
	env := baseEnv.Copy()
	code := "(timed-apply (lambda (x) (+ x x)) 30)"
	sexp, err := ParseString(code)
	if err != nil {
		t.Errorf("Unable to parse code: %v\nCode:\n%s\n", err, code)
	}

	result := Eval(sexp, env)
	listVal, found := result.(List)

	if !found || len(listVal) != 2 || listVal[0] != Integer(60) {
		t.Errorf("Failed to evaluate to the correct value for \"%s\". Got: %v", code, listVal)
	}

	// timed-apply with a lambda and 0 parameters
	env = baseEnv.Copy()
	code = "(timed-apply (lambda () (+ 30 30)))"
	sexp, err = ParseString(code)
	if err != nil {
		t.Errorf("Unable to parse code: %v\nCode:\n%s\n", err, code)
	}

	result = Eval(sexp, env)
	listVal, found = result.(List)

	if !found || len(listVal) != 2 || listVal[0] != Integer(60) {
		t.Errorf("Failed to evaluate to the correct value for \"%s\". Got: %v", code, listVal)
	}

	// timed-apply with a procedure symbol and 0 parameters
	env = baseEnv.Copy()
	code = `(begin 
	(define foo (lambda () (+ 30 30)))
	(timed-apply foo))`
	sexp, err = ParseString(code)
	if err != nil {
		t.Errorf("Unable to parse code: %v\nCode:\n%s\n", err, code)
	}

	result = Eval(sexp, env)
	listVal, found = result.(List)

	if !found || len(listVal) != 2 || listVal[0] != Integer(60) {
		t.Errorf("Failed to evaluate to the correct value for \"%s\". Got: %v", code, listVal)
	}

	// timed-apply with a procedure symbol and 1 parameters
	env = baseEnv.Copy()
	code = `(begin 
	(define foo (lambda (x) (+ x x)))
	(timed-apply foo 30))`
	sexp, err = ParseString(code)
	if err != nil {
		t.Errorf("Unable to parse code: %v\nCode:\n%s\n", err, code)
	}

	result = Eval(sexp, env)
	listVal, found = result.(List)

	if !found || len(listVal) != 2 || listVal[0] != Integer(60) {
		t.Errorf("Failed to evaluate to the correct value for \"%s\". Got: %v", code, listVal)
	}
}

func TestBasicMacros(t *testing.T) {
	// setup the test environment
	baseEnv := NewEnvironment(nil)
	baseEnv.SetupPrimitives()

	// add in some test macros
	code := "(defmacro macro1 (cond conseq alt) `(if ,cond ,conseq ,alt)))"
	sexp, err := ParseString(code)
	if err != nil {
		t.Errorf("Unable to parse macro code: %v\nCode:\n%s\n", err, code)
	}
	Eval(sexp, baseEnv)

	// copy environment to avoid polluting original Environment
	env := baseEnv.Copy()

	doTestEvalForInt(t, env, Integer(10), `(begin 
		(define testBool true)
		(macro1 testBool (+ 5 5) (- 0 10))
		)`)

}
