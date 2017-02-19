// Copyright 2017, Timothy Bogdala <tdb@animal-machine.com>
// See the LICENSE file for more details.

package gimmick

import "time"

func primCons(sexp Value, env *Environment) Value {
	sexpList, isList := sexp.(List)
	if !isList {
		return List{}
	}

	sexpLen := len(sexpList)

	// empty list
	if sexpLen < 2 {
		return List{}
	}

	// new list for the first item, evaluated
	if sexpLen == 2 {
		return List{Eval(sexpList[1], env)}
	}

	// new list for the first item and each of the rest appended
	newList := List{Eval(sexpList[1], env)}
	for _, sexp := range sexpList[2:] {
		newList = append(newList, Eval(sexp, env))
	}
	return newList
}

// (car <list>)
func primCar(sexp Value, env *Environment) Value {
	sexpList, isList := sexp.(List)
	if !isList {
		return List{}
	}

	// empty list
	sexpLen := len(sexpList)
	if sexpLen < 2 {
		return List{}
	}

	// parameter one needs to be a list
	list, isList := Eval(sexpList[1], env).(List)
	if !isList || len(list) < 1 {
		return List{}
	}

	// return the first item of the first parameter
	return list[0]
}

// (cdr <list>)
func primCdr(sexp Value, env *Environment) Value {
	sexpList, isList := sexp.(List)
	if !isList {
		return List{}
	}

	// empty list
	sexpLen := len(sexpList)
	if sexpLen < 2 {
		return List{}
	}

	// parameter one needs to be a list
	list, isList := Eval(sexpList[1], env).(List)
	if !isList || len(list) < 2 {
		return List{}
	}

	// return a slice excluding the first item of the first parameter
	return list[1:]
}

// (+ <int|float>+)
// (+) == 0
func primAdd(sexp Value, env *Environment) Value {
	return primMath(sexp, env,
		func(a Integer, b Integer) Integer {
			return a + b
		},
		func(a Float, b Float) Float {
			return a + b
		})
}

// (- <int|float>+)
// (- <int|float>) == <int|float>
// (-) == 0
func primSub(sexp Value, env *Environment) Value {
	return primMath(sexp, env,
		func(a Integer, b Integer) Integer {
			return a - b
		},
		func(a Float, b Float) Float {
			return a - b
		})
}

// (* <int|float>+)
// (* <int|float>) == <int|float>
// (*) == 0
func primMul(sexp Value, env *Environment) Value {
	return primMath(sexp, env,
		func(a Integer, b Integer) Integer {
			return a * b
		},
		func(a Float, b Float) Float {
			return a * b
		})
}

// (/ <int|float>+)
// (/ <int|float>) == <int|float>
// (/) == 0
func primDiv(sexp Value, env *Environment) Value {
	return primMath(sexp, env,
		func(a Integer, b Integer) Integer {
			return a / b
		},
		func(a Float, b Float) Float {
			return a / b
		})
}

type integerMathOp func(Integer, Integer) Integer
type floatMathOp func(Float, Float) Float

func primMath(sexp Value, env *Environment, mathOpI integerMathOp, mathOpF floatMathOp) Value {
	sexpList, isList := sexp.(List)
	sexpListLen := len(sexpList)
	if !isList || sexpListLen < 2 {
		return Integer(0)
	}

	// branch the addition based on the type of the first number
	// which will determine the result type.
	firstNumber := Eval(sexpList[1], env)
	firstInt, isFirstInt := firstNumber.(Integer)
	if isFirstInt {
		acc := firstInt
		if sexpListLen > 2 {
			for _, subSexp := range sexpList[2:] {
				subNumber := Eval(subSexp, env)
				subInt, isSubInt := subNumber.(Integer)
				if isSubInt {
					acc = mathOpI(acc, subInt)
				} else {
					subFloat, isSubFloat := subNumber.(Float)
					if isSubFloat {
						acc = mathOpI(acc, Integer(subFloat))
					}
				}
			}
		}

		return acc
	}

	firstFloat, isFirstFloat := firstNumber.(Float)
	if !isFirstFloat {
		return Integer(0)
	}

	acc := firstFloat
	if sexpListLen > 2 {
		for _, subSexp := range sexpList[2:] {
			subNumber := Eval(subSexp, env)
			subInt, isSubInt := subNumber.(Integer)
			if isSubInt {
				acc = mathOpF(acc, Float(subInt))
			} else {
				subFloat, isSubFloat := subNumber.(Float)
				if isSubFloat {
					acc = mathOpF(acc, subFloat)
				}
			}
		}
	}

	return acc
}

// (> <int|float>+)
// (> <int|float>) == true
// (>) == true
// otherwise == false
func primGreater(sexp Value, env *Environment) Value {
	return primCmp(sexp, env,
		func(a Integer, b Integer) Bool {
			return a > b
		},
		func(a Float, b Float) Bool {
			return a > b
		})
}

// (< <int|float>+)
// (< <int|float>) == true
// (<) == true
// otherwise == false
func primLesser(sexp Value, env *Environment) Value {
	return primCmp(sexp, env,
		func(a Integer, b Integer) Bool {
			return a < b
		},
		func(a Float, b Float) Bool {
			return a < b
		})
}

// (>= <int|float>+)
// (>= <int|float>) == true
// (>=) == true
// otherwise == false
func primGreaterEq(sexp Value, env *Environment) Value {
	return primCmp(sexp, env,
		func(a Integer, b Integer) Bool {
			return a >= b
		},
		func(a Float, b Float) Bool {
			return a >= b
		})
}

// (<= <int|float>+)
// (<= <int|float>) == true
// (<=) == true
// otherwise == false
func primLesserEq(sexp Value, env *Environment) Value {
	return primCmp(sexp, env,
		func(a Integer, b Integer) Bool {
			return a <= b
		},
		func(a Float, b Float) Bool {
			return a <= b
		})
}

type integerCmpOp func(Integer, Integer) Bool
type floatCmpOp func(Float, Float) Bool

func primCmp(sexp Value, env *Environment, cmpOpI integerCmpOp, cmpOpF floatCmpOp) Value {
	sexpList, isList := sexp.(List)
	sexpListLen := len(sexpList)
	if !isList || sexpListLen < 2 {
		return Bool(true)
	}

	// branch the addition based on the type of the first number
	// which will determine the result type.
	firstNumber := Eval(sexpList[1], env)
	firstInt, isFirstInt := firstNumber.(Integer)
	if isFirstInt {
		left := firstInt
		if sexpListLen > 2 {
			for _, subSexp := range sexpList[2:] {
				subNumber := Eval(subSexp, env)
				subInt, isSubInt := subNumber.(Integer)
				if isSubInt {
					if !cmpOpI(left, subInt) {
						return Bool(false)
					}
					left = subInt
				} else {
					subFloat, isSubFloat := subNumber.(Float)
					if isSubFloat {
						if !cmpOpI(left, Integer(subFloat)) {
							return Bool(false)
						}
					} else {
						// not an int or float
						return Bool(false)
					}
					left = Integer(subFloat)
				}

			}
		}

		return Bool(true)
	}

	firstFloat, isFirstFloat := firstNumber.(Float)
	if !isFirstFloat {
		return Bool(false)
	}

	left := firstFloat
	if sexpListLen > 2 {
		for _, subSexp := range sexpList[2:] {
			subNumber := Eval(subSexp, env)
			subInt, isSubInt := subNumber.(Integer)
			if isSubInt {
				if !cmpOpF(left, Float(subInt)) {
					return Bool(false)
				}
				left = Float(subInt)
			} else {
				subFloat, isSubFloat := subNumber.(Float)
				if isSubFloat {
					if !cmpOpF(left, subFloat) {
						return Bool(false)
					}
				} else {
					// not an int or float
					return Bool(false)
				}
				left = subFloat
			}
		}
	}

	return Bool(true)
}

// (eqv <value> <value>)
// (eqv <value>) == true
// (eqv) == true
// otherwise == false
func primEqv(sexp Value, env *Environment) Value {
	sexpList, isList := sexp.(List)
	sexpListLen := len(sexpList)
	if !isList || sexpListLen < 3 {
		return Bool(true)
	}

	return primEqv2(sexpList[1], sexpList[2], env)
}

func primEqv2(aExp, bExp Value, env *Environment) Value {
	a := Eval(aExp, env)
	b := Eval(bExp, env)

	_, aIsList := a.(List)
	_, bIsList := b.(List)
	if !aIsList && !bIsList {
		result := a == b
		return Bool(result)
	}

	return primEqvList(a, b, env)
}

func primEqvList(a, b Value, env *Environment) Bool {
	aList, aIsList := a.(List)
	bList, bIsList := b.(List)

	// special cases for list comparisons
	if aIsList && bIsList {
		if len(aList) != len(bList) {
			return Bool(false)
		}

		for i, aExp := range aList {
			aVal := Eval(aExp, env)
			bVal := Eval(bList[i], env)
			if primEqv2(aVal, bVal, env) == Bool(false) {
				return Bool(false)
			}
		}

		return Bool(true)
	}

	// if we get here then only one of the values is a list, so this fails
	return Bool(false)
}

// (timed-apply <proc> <args>*)
// will return 0.0 on errors
func primTimedApply(sexp Value, env *Environment) Value {
	sexpList, isList := sexp.(List)
	sexpListLen := len(sexpList)
	if !isList || sexpListLen < 2 {
		return Float(0.0)
	}

	// find the function to apply. this will either be a
	// symbol in the environment or a function to call
	var proc Value
	sym, isSymbol := sexpList[1].(Symbol)
	if isSymbol {
		var foundProc bool
		proc, foundProc = env.Find(sym)
		if !foundProc {
			return Float(0.0)
		}
	} else {
		proc = Eval(sexpList[1], env)
	}

	// is it a user defined function
	procedure, isProcedure := proc.(Procedure)
	if !isProcedure {
		return Float(0.0)
	}
	// make sure it's called with the same number of arguments
	if len(sexpList)-2 != len(procedure.Args) {
		return Float(0.0)
	}

	pEnv := NewEnvironment(procedure.ParentEnv)
	for argI, argV := range procedure.Args {
		pEnv.Vars[argV.(Symbol)] = Eval(sexpList[argI+2], env)
	}

	start := time.Now()
	result := Eval(procedure.Body, pEnv)
	finish := time.Now()

	retList := List{result, Float(finish.Sub(start).Seconds())}
	return retList
}
