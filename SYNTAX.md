Gimmick Syntax Reference
========================

Basic Language Concepts
-----------------------

All expressions in Gimmick are written in sexp (symbolic expression) form. 

```lisp
(begin 
  (define r 10) 
  (+ r 5))
```

A sexp is a list. The first item in the sexp list can be a special form, a symbol or
an atom.

If the first item is an atom, then the list is considered to be only data. Otherwise if
it is a symbol that matches a procedure, then that procedure will be called with the rest
of the list elements passed as arguments. If the first item was a special form, the 
behavior will be similar to a symbol matching a procedure, but it will be evaluated 
with special conditions that are defined by each of the special forms. If the first item
is a sexp that evaluates to a lambda, that lambda can then be invoked with the rest of the
list acting as parameters to that lambda.

If the first sexp in a list is something other than a symbol, atom, special form or
a lambda form, the result is undefined.


Data Types
----------

* Integer
* Float
* Bool
* String
* Symbol
* List
* Primitive
* Procedure

Currently all numeric types are 64 bit and can either be integer or floating point.


Special Forms
-------------

The following are the current special forms supported by Gimmick:


### special-form begin: _(begin sexp*)_

The `begin` special form is used to evaluate expressions sequentially in a left-to-right fashion.


### special-form define: _(define symbol sexp)_

The `define` special form evaluates `sexp` and then assigns the result to a memory location identified by the `symbol`.


### special-form if: _(if test-sexp consequent-sexp alternative-sexp)_

The `if` special form evaluates `test-sexp` and based on the true/false value will either evaluate the `consequent-sexp` or
the `alternative-sexp` expressions, but not both.


### special-form quote: _(quote sexp)_

The `quote` special form simply returns the expression without evaluation. `(quote true)` returns the bool `true`.
`(quote (+ 5 5))` returns a list value with three items: symbol `+`, integer `5`, integer `5`.


### special-form let: _(let binding-pairs body)_

The `let` special form creates a new evaluation environment for `body` with variables set from `binding-pairs`, which
must be a list that contains list pairs each of which is a symbol and an expression. Each symbol gets bound to the 
evaluated expression before `body` is evaluated. The following example evaluates to `35`:

```lisp
(let ((x 2) (y 3))
  (let ((x 7)
        (z (+ x y)))
    (* z x)))
```

### special-form let*: _(let* binding-pairs body)_

The `let*` special form behaves like `let` except each binding pair is evaluated in the new environment for `body`
so that the binding pairs can reference symbols that came before it in the `binding-pairs` list. The difference can
be illustrated by this expression which evaluates to `70`:

```lisp
(let ((x 2) (y 3))
  (let* ((x 7)
         (z (+ x y)))
    (* z x)))
```

### special-form letrec: _(letrec binding-pairs body)_

The `letrec` special form behaves like `let*` except that all of the symbols in `binding-pairs` are added to
the new environment first with an undefined value so that any of the binding symbols can be referenced
in the binding expressions. The following expression illustrates how this allows for recursive calls
between bindings and evaluates to `true`:

```lisp
(letrec ((zero? (lambda (x) (eqv? x 0)))
         (even? (lambda (n) (if (zero? n) true (odd? (- n 1)))))
         (odd?  (lambda (n) (if (zero? n) false (even? (- n 1))))))
  (even? 88))
``` 

### special-form quasiquote: _(quasiquote sexp)_
### special-form unquote: _(unquote sexp)_
### special-form unquote-splicing: _(unquote-splicing sexp)_

The `quasiquote` special form behaves similarly to `quote` but has a special form called `unquote` that allows for selective
evaluation of expressions within the `sexp` parameter. 

A simple example of this is the sexp `(quasiquote (5 (unquote (+ 2 3))))` which evaluates to a list `(5 5)`. The first 5
is quoted and therefore returned without evaluation. The second item in the list passed to `quasiquote` is an `unquote`
expression which evaluates to the integer 5 which is then returned.

The results of unquoting can be spliced into the result using the special form `unquote-splicing`. An example of
this is the following expression: `(quasiquote (1 (unquote-splicing (list 2 3)) 4 5))` which evaluates to
`(1 2 3 4 5)`.

Because the terms `quasiquote`, `unquote` and `unquote-splicing` appear to be verbose in the use cases
for these special forms, special syntax is introduced. The back-quote "\`" character can be used for `quasiquote`,
the comma "," character can be used for `unquote` and a comma-ampersand combo ",@" can be used for `unquote-splicing`.
An example of this syntax is:

```lisp
`(,x ,y ,@z)
```

### special-form lambda: _(lambda param-symbol-list body-sexp)_

The `lambda` special form is used to create procedures that can be called with a fixed or
variable number of arguments. If the three forms are not supplied (`lambda`, param list and body expression) the lambda expression evaluates to an empty list.

The `param-symbol-list` should be a list of symbols or an empty list. If any item in the `param-symbol-list` is not
a symbol, then the lambda expression evaluates to an empty list.

An exception to this is that a "." can be supplied with a symbol following it that will be a list containing all of the rest of the lambda parameters in the call. The following example will
evaluate to `(1 2 3 4 5)`:

```lisp
(define foo 
  (lambda (x y . z)
    `(,x ,y ,@z)))
(foo 1 2 3 4 5)))
```

The `lambda` special form then creates a new environment to evaluate `body-sexp` in with
bindings for all symbols in `param-symbol-list` to the evaluated expressions passed into the lambda.
These symbol bindings can shadow previously existing symbols. In order to evaluate multiple
expressions, a `begin` form can be used to sequence operations.

### special-form defmacro: _(defmacro name param-symbol-list body-sexp)_

The `defmacro` special form is used to create program code at evaluation time by evaluating
to a sexp form. The `name` parameter should be a symbol or `defmacro` evaluates to an empty
list. `param-symbol-list` must be a list of symbols or the "." character as defined in `lambda`
or else `defmacro` evaluates to an empty list.

When a macro is encountered during evaluation a new environment and parameter bindings
will be created similarly to `lambda` with the key difference that the sexp parameters **are not evaluated**
before binding them to the parameter symbols in the new environment. The macro is then evaluated 
and then the result of the macro is then evaluated in the same environment. The following macro
will evaluate to the integer `10`.

```lisp
(defmacro macro1 (cond conseq alt) `(if ,cond ,conseq ,alt)))
(define testBool true)
(macro1 testBool (+ 5 5) (- 0 10))
```


Operators
---------

Math functions are defined to take a variable number of parameters and can intermix Integer and Float types.
The resulting value will match the type of the first parameter to the math operation.

* \+
* \-
* \*
* \/

Comparisons are also defined with the possibility to intermix Integer and Float types.

* \<
* \>
* \<\=
* \>\=

Equality testing is defined with `eqv?`. If the parameters to `eqv?` are both lists, then a `true` result
is returned if the lengths are equal and each item in each list evaluate to an equal value; otherwise
`false` is returned. If only one parameter is a list, `false` is also returned.

Calling `eqv?` with only one parameter will return `true`.


Library Functions
-----------------

The following are built-in functions to the standard environment.

### lambda list: _(list sexp*)_

The `list` function returns a new list object. If no parameters are supplied an empty list is returned. Otherwise,
a new list object is created and each `sexp` passed in will be evaluated with the result stored in the new list.
`(list (+ 1 2) 4)` will evaluate to `(3 4)`. 

### lambda car: _(car sexp)_

The `car` function returns the first item of the list `sexp` passed in. If the `sexp` does not
evaluate to a list or if the `sexp` evaluates to an empty list, an empty list value is returned. 
Otherwise the first sexp in the list is returned. `(car (list 1 2 3))` evaluates to `1`.

### lambda cdr: _(cdr sexp)_

The `cdr` function returns all items except the firstof the list `sexp` passed in. 
If the `sexp` does not evaluate to a list or if the `sexp` evaluates to an empty list, an empty list value is returned. Otherwise a list is returned with all items of the `sexp` list except
the first. `(cdr (list 1 2 3))` evaluates to `(2 3)`.

### lambda timed-apply: _(timed-apply proc args*)_

The `timed-apply` function will call the lambda identified by the symbol `proc` with the parameters
to that function being the rest of the parameters to `timed-apply`. The duration of the evaluation
will be recorded and `timed-apply` will evaluate to a list where the first element is the result
of the `proc` lambda and the second element is the duration of evaluation in seconds.

The following example might evaluate to something like `(60 6.3885e-05)`.

```lisp
(begin 
  (define foo (lambda (x) (+ x x)))
  (timed-apply foo 30))
```


Differences from Scheme
-----------------------

Some of these are by design and some of these are just current limitations. This list
may also be incomplete.

* variables are strongly typed
* #t/#f are `true` and `false`
* no continuations
* no error support