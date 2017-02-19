// Copyright 2017, Timothy Bogdala <tdb@animal-machine.com>
// See the LICENSE file for more details.

package gimmick

// Environment represents the symbol mappings for a given context; can be nested.
type Environment struct {
	// Vars are the symbol mappings for the environment context
	Vars map[Symbol]Value

	// Parent is the enclosing environment object
	Parent *Environment
}

// NewEnvironment creates a new environment context with an optional parent context.
func NewEnvironment(parent *Environment) *Environment {
	env := new(Environment)
	env.Vars = make(map[Symbol]Value)
	env.Parent = parent
	return env
}

// Copy returns a new copy of the Environment with a separate Vars map so
// that changes to the copy do not affect the Vars of the original Environment.
func (env *Environment) Copy() *Environment {
	copy := NewEnvironment(env.Parent)
	for k, v := range env.Vars {
		copy.Vars[k] = v
	}
	return copy
}

// Find attempts to find the symbol in the current environment mapping or
// it's parent environment. Returns a bool indicating if the Value was found.
func (env *Environment) Find(s Symbol) (Value, bool) {
	v, found := env.Vars[s]
	if found {
		return v, true
	}

	// not found; check the Parent
	if env.Parent != nil {
		return env.Parent.Find(s)
	}

	// total failure
	return nil, false
}

// SetupPrimitives will add in the primitive functions to the given environment.
func (env *Environment) SetupPrimitives() {
	env.Vars["+"] = Primitive(primAdd)
	env.Vars["-"] = Primitive(primSub)
	env.Vars["*"] = Primitive(primMul)
	env.Vars["/"] = Primitive(primDiv)
	env.Vars["<"] = Primitive(primLesser)
	env.Vars["<="] = Primitive(primLesserEq)
	env.Vars[">"] = Primitive(primGreater)
	env.Vars[">="] = Primitive(primGreaterEq)
	env.Vars["eqv?"] = Primitive(primEqv)
	env.Vars["cons"] = Primitive(primCons)
	env.Vars["car"] = Primitive(primCar)
	env.Vars["cdr"] = Primitive(primCdr)
	env.Vars["timed-apply"] = Primitive(primTimedApply)
}
