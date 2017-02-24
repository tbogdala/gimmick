// Copyright 2017, Timothy Bogdala <tdb@animal-machine.com>
// See the LICENSE file for more details.

/*
gimmick is a lisp-like interpretter for Go.

Current Features
----------------

* Basic sexp parsing


TODO
----

* All `define` operations work on the global environment`


Ideas
-----

Define interfaces with a form like
	(def-interface (<ifunc-defs>))
	(def-ifunc <name> <param>*)
In this way, basic interfaces for math can be defined with global
functions. The compiler would need to keep track of these definitions,
then a function like + could be something like:
	(func + first:addInterface second:addInterface)

Bugs
----

* Multiple newlines in source will not advance line numbers correctly


*/

package gimmick

// Value is the common interface between scheme data types (e.g. number, symbol, lists)
type Value interface{}

// Float is the type for floating point values
type Float float64

// Integer is the type for integer values
type Integer int64

// Bool is a boolean true or false value
type Bool bool

// String is the string type value
type String string

// Symbol is the type for the symbol values
type Symbol string

// List is the list datatype that contains multiple values
type List []Value

// Primitive is a built in function
type Primitive func(Value, *Environment) Value

// Procedure is a user defined function
type Procedure struct {
	//ParentEnv is the environment the procedure was defined in
	ParentEnv *Environment

	// Body is sexp that will be evaluated when the procedure is called
	Body Value

	// Args is the list of procedure arguments accepted
	Args List

	VariableParameters bool
}

// enumeration values for the parsed tokens
const (
	tokenILLEGAL = iota
	tokenSYMBOL
	tokenINTEGER
	tokenFLOAT
	tokenBOOL
	tokenSTRING
	tokenOPENPARENS
	tokenCLOSEPARENS
	tokenQUASIQUOTE
	tokenUNQUOTE
	tokenUNQUOTESPLICING
)

// token is the structure representing each token read in
// and has some attached debugging information to pinpoint
// the token in the source code.
type token struct {
	// Token is the string representation of the token in source code
	Token string

	// Type is the type of the token processed
	Type int

	// Filename is the name of the file where token occurs
	Filename string

	// LineNumber is the line number in the source file as processed where
	// the Token occurs
	LineNumber int
}

// GetTypeString returns the name of the type for the Value object.
func GetTypeString(v Value) string {
	switch v.(type) {
	case Float:
		return "FLOAT"
	case Integer:
		return "INTEGER"
	case Bool:
		return "BOOL"
	case String:
		return "STRING"
	case Symbol:
		return "SYMBOL"
	case List:
		return "LIST"
	case Primitive:
		return "PRIMITIVE"
	case Procedure:
		return "PROCEDURE"
	default:
		return "UNKNOWN"
	}
}
