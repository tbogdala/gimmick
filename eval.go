// Copyright 2017, Timothy Bogdala <tdb@animal-machine.com>
// See the LICENSE file for more details.

package gimmick

// Eval evalutes a sexp value in a given environment and returns the result.
func Eval(v Value, env *Environment) Value {
	for {
		switch e := v.(type) {
		case Symbol:
			s, sFound := env.Find(e)
			if sFound {
				return s
			}
			return List{}
		case Integer:
			return e
		case Float:
			return e
		case Bool:
			return e
		case String:
			return e
		case List:
			// handle some special forms
			if len(e) == 0 {
				return e // empty list
			}

			car, found := e[0].(Symbol)
			if !found {
				return e[0] // int/float
			}

			// handle the special forms
			if car == "quote" {
				// special form quote just returns the following expression
				// without evaluating it. (quote) -> ()
				if len(e) < 2 {
					return List{}
				}
				return e[1]
			} else if car == "quasiquote" {
				// must be in form of (quasiquote <list>)
				if len(e) != 2 {
					return List{}
				}
				listSexp, isList := e[1].(List)
				if !isList {
					return List{}
				}

				return expandQuasiquote(listSexp, env)
			} else if car == "define" {
				// special form to modify the environment
				if len(e) < 3 {
					return List{}
				}
				sym, foundSym := e[1].(Symbol)
				if !foundSym {
					return List{}
				}
				e2 := Eval(e[2], env)
				env.Vars[sym] = e2
				return e2
			} else if car == "begin" {
				// special form of begin is tail-call compatible here
				if len(e) < 2 {
					return List{}
				}
				// tail-call update to value being evaluated in the loop
				// the last sub sexp evaluated will remain as v on loop restart
				var val Value
				for _, subSexp := range e[1:] {
					val = Eval(subSexp, env)
				}
				return val
			} else if car == "lambda" {
				// special form for defining functions
				if len(e) < 3 {
					return List{}
				}

				// check the second item in the list to make
				// sure it's a list of symbols. if it's not, just
				// return an empty list.
				argList, isArgList := e[1].(List)
				if !isArgList {
					return List{}
				}
				for _, argItem := range argList {
					if _, isArgSymbol := argItem.(Symbol); !isArgSymbol {
						return List{}
					}
				}

				// build the procedure object
				var proc Procedure
				proc.ParentEnv = env
				proc.Args = argList
				proc.Body = e[2]
				return proc
			} else if car == "if" {
				// special form for if. If the condition doesn't evaluate
				// to a bool, then neither conseq or altern get evaluated and
				// an empty list is returned.
				if len(e) <= 2 {
					// must atleast have (if <cond> <conseq>)
					return List{}
				}

				cond := Eval(e[1], env)
				condBool, isCondBool := cond.(Bool)
				if !isCondBool {
					return List{}
				}

				// tail-call update the value being evaulated in the loop
				if condBool {
					v = e[2]
				} else if len(e) >= 4 {
					v = e[3]
				} else {
					return List{} // false, but no alt sexp
				}
			} else {
				// check the environment for a procedure
				proc, found := env.Find(car)
				if found {
					// is it a built in function
					prim, isPrimitive := proc.(Primitive)
					if isPrimitive {
						return prim(e, env)
					}

					// is it a user defined function
					procedure, isProcedure := proc.(Procedure)
					if isProcedure {
						// make sure it's called with the same number of arguments
						if len(e)-1 != len(procedure.Args) {
							return List{}
						}
						pEnv := NewEnvironment(procedure.ParentEnv)
						for argI, argV := range procedure.Args {
							pEnv.Vars[argV.(Symbol)] = Eval(e[argI+1], env)
						}

						// tail-call support means we don't Eval the procedure.Body here,
						// and instead update v and env and restart the loop
						v = procedure.Body
						env = pEnv
					}
				} else {
					// procedure wasn't found in the environment
					return List{}
				}
			}
		}
	}
}

// expandQuasiquote can be called recurisvely while expanding a quasiquote sexp.
//
// quasiquote has special functions to evaluate within the list:
//	* unquote
//  * unquote-splicing
func expandQuasiquote(listSexp List, env *Environment) Value {
	// quote would return this: the list param without eval.
	return listSexp
}
