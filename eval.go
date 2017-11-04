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
				// if the first item in a list is not a symbol, see if it evaluates to a
				// procedure that can then be called
				evaluatedFirst := Eval(e[0], env)
				firstAsProc, foundProc := evaluatedFirst.(Procedure)
				if foundProc {
					// make sure it's called with the same number of arguments if
					// the procedure doesn't support variable number of arguments
					if !firstAsProc.VariableParameters && (len(e)-1 != len(firstAsProc.Args)) {
						return List{}
					}

					pEnv := bindNewEnvForProc(&firstAsProc, e, env)

					// tail-call support means we don't Eval the procedure.Body here,
					// and instead update v and env and restart the loop
					v = firstAsProc.Body
					env = pEnv
					continue
				}

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
			} else if car == "let" || car == "let*" || car == "letrec" {
				if len(e) < 3 {
					return List{}
				}

				pEnv := NewEnvironment(env)

				// the first parameter to let should be a list of pairs, with
				// each pair being a (symbol sexp) form. any violation of this
				// and an empty list is returned.
				argList, isArgList := e[1].(List)
				if !isArgList {
					return List{}
				}

				// if this is a letrec, then go through the arglist and bind
				// empty list values for all of the parameters so that the
				// symbols exist in the environment.
				if car == "letrec" {
					for _, argItem := range argList {
						argItemList, isArgItemList := argItem.(List)
						if !isArgItemList {
							return List{}
						} else if len(argItemList) != 2 {
							return List{}
						}

						letSym, foundSym := argItemList[0].(Symbol)
						if !foundSym {
							return List{}
						}

						pEnv.Vars[letSym] = List{}
					}
				}

				for _, argItem := range argList {
					argItemList, isArgItemList := argItem.(List)
					if !isArgItemList {
						return List{}
					} else if len(argItemList) != 2 {
						return List{}
					}

					letSym, foundSym := argItemList[0].(Symbol)
					if !foundSym {
						return List{}
					}

					// default case is `let` where each binding is evaluated in the
					// parent environment, unaffected by bindings currently being set.
					evalEnv := env
					if car != "let" {
						// let* and letrec parameter expressions are evaluated in an environment
						// that has the previous variables in the list bound.
						evalEnv = pEnv
					}

					// let parameter expressions are evaluated in current environment
					letVal := Eval(argItemList[1], evalEnv)
					// and then the binding is set in the new environment
					pEnv.Vars[letSym] = letVal
				}

				// now adjust the evaluating environment
				v = e[2]
				env = pEnv

			} else if car == "lambda" {
				var proc Procedure

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
					if argItem == Symbol(".") {
						proc.VariableParameters = true
					}
				}

				// build the procedure object
				proc.ParentEnv = env
				proc.Args = argList
				proc.Body = e[2]
				return proc
			} else if car == "defmacro" {
				// special form to expand symbolic expressions at runtime.
				// (macro name (<arg-name>*) <body>)
				if len(e) < 4 {
					return List{}
				}

				// name should be a symbol
				macroName, isNameFound := e[1].(Symbol)
				if !isNameFound {
					return List{}
				}

				var macro Procedure

				// check the third item in the list to make
				// sure it's a list of symbols. if it's not, just
				// return an empty list.
				argList, isArgList := e[2].(List)
				if !isArgList {
					return List{}
				}
				for _, argItem := range argList {
					if _, isArgSymbol := argItem.(Symbol); !isArgSymbol {
						return List{}
					}
					if argItem == Symbol(".") {
						macro.VariableParameters = true
					}
				}

				// build the macro object
				macro.ParentEnv = env
				macro.Args = argList
				macro.Body = e[3]

				// update the environment for the new macro definition
				env.Macros[macroName] = macro

				return macro
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
				// check the environment to see if it's a macro
				// NOTE: this is possible
				macro, isMacro := env.FindMacro(car)
				if isMacro {
					// make sure it's called with the same number of arguments if
					// the procedure doesn't support variable number of arguments
					if !macro.VariableParameters && (len(e)-1 != len(macro.Args)) {
						return List{}
					}
					pEnv := NewEnvironment(macro.ParentEnv)

					variableMode := false

					for argI, argV := range macro.Args {
						if argV == Symbol(".") {
							variableMode = true
							continue
						}
						if variableMode {
							// if we have variable parameters, then all the rest of the
							// arguments passed into the lambda get bound to the symbol
							// right afer the "." symbol as a list. The symbol is this argV
							// if variableMode is true.
							rest := List{}
							for i := argI; i < len(e); i++ {
								rest = append(rest, e[i])
							}
							pEnv.Vars[argV.(Symbol)] = rest
						} else {
							pEnv.Vars[argV.(Symbol)] = e[argI+1]
						}
					}

					// evaluate the macro to expand and return the result
					retVal := Eval(macro.Body, pEnv)

					// since this is a macro we then evaluate the result
					retVal = Eval(retVal, env)
					return retVal
				}

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
						// make sure it's called with the same number of arguments if
						// the procedure doesn't support variable number of arguments
						if !procedure.VariableParameters && (len(e)-1 != len(procedure.Args)) {
							return List{}
						}

						pEnv := bindNewEnvForProc(&procedure, e, env)

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

func bindNewEnvForProc(procedure *Procedure, e List, env *Environment) *Environment {
	pEnv := NewEnvironment(procedure.ParentEnv)

	variableMode := false

	for argI, argV := range procedure.Args {
		if argV == Symbol(".") {
			variableMode = true
			continue
		}
		if variableMode {
			// if we have variable parameters, then all the rest of the
			// arguments passed into the lambda get bound to the symbol
			// right after the "." symbol as a list. The symbol is this argV
			// if variableMode is true.
			rest := List{}
			for i := argI; i < len(e); i++ {
				rest = append(rest, Eval(e[i], env))
			}
			pEnv.Vars[argV.(Symbol)] = rest
		} else {
			pEnv.Vars[argV.(Symbol)] = Eval(e[argI+1], env)
		}
	}

	return pEnv
}

// expandQuasiquote can be called recursively while expanding a quasiquote sexp.
// quasiquote has special functions to evaluate within the list:
//	* unquote
//  * unquote-splicing
func expandQuasiquote(listSexp List, env *Environment) Value {
	// initial call of this function will have listSexp == the entire
	// list to expand.
	//
	// e.g. (quasiquote (1 (unquote (+ 5 5)) 21))
	// or ... `(1 ,(+ 5 5) 21)
	// and this function gets listSexp of: (1 (unquote (+ 5 5)) 21)

	// loop through all items in the list to see if there's something
	// to unquote or unquote-splice.
	newList := List{}
	for _, sexp := range listSexp {
		// check to see if it's a list. if it's not a list,
		// then the rest of the things pass unmodified.
		subList, isList := sexp.(List)
		if isList && len(subList) > 0 {
			// if it is a list, see if it starts with a symbol we treat specially
			firstSymbol, firstIsSymbol := subList[0].(Symbol)
			if firstIsSymbol {
				switch firstSymbol {
				case "unquote":
					if len(subList) > 1 {
						newList = append(newList, Eval(subList[1], env))
					}
				case "unquote-splicing":
					if len(subList) > 1 {
						splice := Eval(subList[1], env)
						spliceList, isSpliceList := splice.(List)
						if isSpliceList {
							for _, spliceItem := range spliceList {
								newList = append(newList, spliceItem)
							}
						}
					}
				case "quasiquote":
					if len(subList) > 1 {
						subSubList, isSubSubList := subList[1].(List)
						if isSubSubList {
							newList = append(newList, expandQuasiquote(subSubList, env))
						}
					}
				default:
					// we've determined it's not a special list, so pass it on
					// to a recursive call to see if we got any other special
					// comands to process.
					newList = append(newList, expandQuasiquote(subList, env))
				}
			}
		} else {
			// not a list, so pass it on
			newList = append(newList, sexp)
		}
	}

	return newList
}
