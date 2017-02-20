// Copyright 2017, Timothy Bogdala <tdb@animal-machine.com>
// See the LICENSE file for more details.

package gimmick

import (
	"testing"
)

func doParseTestWithKnownGood(t *testing.T, code string, knownGood []token) {
	// tests multi-line strings as well
	tokens := buildTokens(code)

	// test the tokens against known good copies
	if len(knownGood) != len(tokens) {
		t.Errorf("Amount of known good tokens is different from tokens generated (%d vs %d)", len(knownGood), len(tokens))
		return
	}
	for i, tok := range tokens {
		if tok.Type != knownGood[i].Type {
			t.Errorf("Known good token type is different from generated token (%d vs %d) for token #%d", knownGood[i].Type, tok.Type, i)
		}
		if tok.Token != knownGood[i].Token {
			t.Errorf("Known good token is different from generated token (%s vs %s) for token #%d", knownGood[i].Token, tok.Token, i)
		}
		if tok.LineNumber != knownGood[i].LineNumber {
			t.Errorf("Known good token line number is different from generated token (%d vs %d) for token #%d", knownGood[i].LineNumber, tok.LineNumber, i)
		}
	}
}

func TestBasicEval(t *testing.T) {
	// setup an environment with a test value for 'x'
	env := NewEnvironment(nil)
	env.Vars[Symbol("x")] = Integer(42)

	// Test: evaluate a symbol expression that should evaluate to an int
	code := "x"
	sexp, err := ParseString(code)
	if err != nil {
		t.Errorf("Failed to parse symbol test expression.")
	}
	result := Eval(sexp, env)

	intVal, found := result.(Integer)
	if !found || intVal != 42 {
		t.Errorf("Failed to find the test symbol and get the correct value.")
	}

	// Test: evaluate an integer expression
	code = "84"
	sexp, err = ParseString(code)
	if err != nil {
		t.Errorf("Failed to parse integer test expression.")
	}
	result = Eval(sexp, env)
	intVal, found = result.(Integer)
	if !found || intVal != 84 {
		t.Errorf("Failed to evaluate an integer and/or get the correct value.")
	}

	// Test evaluate a float expression
	code = "6.66"
	sexp, err = ParseString(code)
	if err != nil {
		t.Errorf("Failed to parse integer test expression.")
	}
	result = Eval(sexp, env)
	floatVal, found := result.(Float)
	if !found || floatVal != 6.66 {
		t.Errorf("Failed to evaluate a float and/or get the correct value.")
	}

	// Test evaluate a simple define  form
	code = "(define y 100)"
	sexp, err = ParseString(code)
	if err != nil {
		t.Errorf("Failed to parse define special form expression.")
	}
	result = Eval(sexp, env)
	intVal, found = result.(Integer)
	if !found || intVal != 100 {
		t.Errorf("Failed to evaluate an define form and get the value back.")
	}
	if env.Vars[Symbol("y")] != Integer(100) {
		t.Errorf("Failed to retrieve a defined variable from the environment variable.")
	}

	// Test evaluate a bool expression
	code = "true"
	sexp, err = ParseString(code)
	if err != nil {
		t.Errorf("Failed to parse bool test expression.")
	}
	result = Eval(sexp, env)
	boolVal, found := result.(Bool)
	if !found || boolVal != true {
		t.Errorf("Failed to evaluate a bool and/or get the correct value.")
	}

	// Test evaluate a string expression
	code = "\"REDRUM\""
	sexp, err = ParseString(code)
	if err != nil {
		t.Errorf("Failed to parse string test expression.")
	}
	result = Eval(sexp, env)
	stringVal, found := result.(String)
	if !found || stringVal != "REDRUM" {
		t.Errorf("Failed to evaluate a string and/or get the correct value.")
	}
}

func TestBasicParsing(t *testing.T) {
	code := `(begin
(define r 10)
(* pi (* r r)))`

	sexp, err := ParseString(code)
	if err != nil {
		t.Errorf("Unable to parse code: %v\nCode:\n%s\n", err, code)
	}

	knownGood := List{Symbol("begin"),
		List{Symbol("define"), Symbol("r"), Integer(10)},
		List{Symbol("*"), Symbol("pi"),
			List{Symbol("*"), Symbol("r"), Symbol("r")}}}

	// get our original parsed code as a list
	sexpList, found := sexp.(List)
	if !found {
		t.Errorf("Failed to get a parsed List for the code.")
		return
	}

	// make sure we have a known good quantity here
	if cmpList(sexpList, knownGood) == false {
		t.Errorf("Parsed expression doesn't match known good list.")
		t.Logf("Parsed: %v", sexp)
		t.Logf("Known good: %v", knownGood)
	}

	// add a bad symbol to the end of the list to make sure comparison works
	knownBad := append(knownGood[:], Symbol("FailMe"))
	if cmpList(sexpList, knownBad) == true {
		t.Errorf("Parsed expression should be diffent from test data but it's not.")
		t.Logf("Parsed: %v", sexp)
		t.Logf("Known bad: %v", knownBad)
	}
}

func cmpList(one List, two List) bool {
	if len(one) != len(two) {
		return false
	}

	for i, oneVal := range one {
		// recursively compare lists
		oneList, oneFound := oneVal.(List)
		if oneFound {
			twoList, found := two[i].(List)
			if !found {
				return false // type mismatch
			}
			if cmpList(oneList, twoList) == false {
				return false // lists didn't match
			}
		} else {
			// otherwise try a straight compare
			if oneVal != two[i] {
				return false
			}
		}
	}

	return true
}

func TestBasicTokenSingleline(t *testing.T) {
	code := "(begin (define r 10) (define rf 10.0) (define t true) (* pi (* r r)))"
	knownGood := []token{}
	knownGood = append(knownGood, token{Token: "(", Type: tokenOPENPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "begin", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "(", Type: tokenOPENPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "define", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "r", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "10", Type: tokenINTEGER, LineNumber: 1})
	knownGood = append(knownGood, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "(", Type: tokenOPENPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "define", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "rf", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "10.0", Type: tokenFLOAT, LineNumber: 1})
	knownGood = append(knownGood, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "(", Type: tokenOPENPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "define", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "t", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "true", Type: tokenBOOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "(", Type: tokenOPENPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "*", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "pi", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "(", Type: tokenOPENPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "*", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "r", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "r", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: 1})

	doParseTestWithKnownGood(t, code, knownGood)
}

func TestBasicTokenMultiline(t *testing.T) {
	code := `(begin

(define r 10)


(* pi (* r r)))`

	knownGood := []token{}
	knownGood = append(knownGood, token{Token: "(", Type: tokenOPENPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "begin", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "(", Type: tokenOPENPARENS, LineNumber: 3})
	knownGood = append(knownGood, token{Token: "define", Type: tokenSYMBOL, LineNumber: 3})
	knownGood = append(knownGood, token{Token: "r", Type: tokenSYMBOL, LineNumber: 3})
	knownGood = append(knownGood, token{Token: "10", Type: tokenINTEGER, LineNumber: 3})
	knownGood = append(knownGood, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: 3})
	knownGood = append(knownGood, token{Token: "(", Type: tokenOPENPARENS, LineNumber: 6})
	knownGood = append(knownGood, token{Token: "*", Type: tokenSYMBOL, LineNumber: 6})
	knownGood = append(knownGood, token{Token: "pi", Type: tokenSYMBOL, LineNumber: 6})
	knownGood = append(knownGood, token{Token: "(", Type: tokenOPENPARENS, LineNumber: 6})
	knownGood = append(knownGood, token{Token: "*", Type: tokenSYMBOL, LineNumber: 6})
	knownGood = append(knownGood, token{Token: "r", Type: tokenSYMBOL, LineNumber: 6})
	knownGood = append(knownGood, token{Token: "r", Type: tokenSYMBOL, LineNumber: 6})
	knownGood = append(knownGood, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: 6})
	knownGood = append(knownGood, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: 6})
	knownGood = append(knownGood, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: 6})

	doParseTestWithKnownGood(t, code, knownGood)
}

func TestBasicLambda(t *testing.T) {
	code := "(lambda (x) (+ x 1))"
	knownGood := []token{}
	knownGood = append(knownGood, token{Token: "(", Type: tokenOPENPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "lambda", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "(", Type: tokenOPENPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "x", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: 1})

	knownGood = append(knownGood, token{Token: "(", Type: tokenOPENPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "+", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "x", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "1", Type: tokenINTEGER, LineNumber: 1})
	knownGood = append(knownGood, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: 1})

	doParseTestWithKnownGood(t, code, knownGood)
}

func TestBasicStringsTest(t *testing.T) {
	// tests multi-line strings as well
	code := `(begin (define str "hello world!") (define str2 "what is
up"))`

	knownGood := []token{}
	knownGood = append(knownGood, token{Token: "(", Type: tokenOPENPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "begin", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "(", Type: tokenOPENPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "define", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "str", Type: tokenSYMBOL, LineNumber: 1})

	knownGood = append(knownGood, token{Token: "hello world!", Type: tokenSTRING, LineNumber: 1})
	knownGood = append(knownGood, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "(", Type: tokenOPENPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "define", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "str2", Type: tokenSYMBOL, LineNumber: 1})

	knownGood = append(knownGood, token{Token: "what is\nup", Type: tokenSTRING, LineNumber: 1})
	knownGood = append(knownGood, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: 1})

	doParseTestWithKnownGood(t, code, knownGood)

	// test #2 is simpler ... just a define
	code = "(define str \"hello world!\")"
	knownGood = []token{}
	knownGood = append(knownGood, token{Token: "(", Type: tokenOPENPARENS, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "define", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "str", Type: tokenSYMBOL, LineNumber: 1})
	knownGood = append(knownGood, token{Token: "hello world!", Type: tokenSTRING, LineNumber: 1})
	knownGood = append(knownGood, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: 1})

	doParseTestWithKnownGood(t, code, knownGood)
}
