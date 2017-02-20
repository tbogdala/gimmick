GIMMICK v0.1.0-development
==========================

Gimmick is a lisp language that aims to feature static typing, first class concurrency support
and native code generation on top of a classic Scheme-like core. 

At present, all of these features are missing and only a basic interpreter exists
that can be called from within a [Go][golang] program.


Requirements
------------

* [readline][chzyer] - a Go library implementing readline features for the REPL


Installation
------------

The dependency Go libraries can be installed with the following commands.

```bash
go get github.com/chzyer/readline
go get github.com/tbogdala/gimmick
```

Usage
-----

First, build the main tool: `gimmick`.

```bash
cd $GOPATH/src/github.com/tbogdala/gimmick/cmd/gimmick
go build
```

Once this executable is built, it can be called with a command-line
parameter to indicate the operation to perform. At present only the `repl`
command is supported.

### REPL

To start the REPL interpreter, run the following:

```bash
./gimmick repl <file_to_load.gmk>*
```

At this point, you can enter in code and evaluate it in a multiline fashion.

The following special commands are supported:

* `.help` prints the list of special commands
* `.parse` shows the parse message for the current sexp
* `.print` displays the current sexp being entered 
* `.quit` exits the REPL
* `.reset` resets the environment currently used for evaluation
* `.type` shows the type of the last result value
* `[empty line]` erases the current command in a multi-line context


Interpreter Features
---------------------

* basic interpreter easily callable from [Go][golang]
* uses symbolic expressions (sexp) for code and data like a lisp
* supports types: int64, float64, boolean, lists, symbols and strings
* basic math operations: `+`, `-`, `*`, `/`
* basic comparisons: `<`, `>`, `<=`, `>=`, `eqv?`
* basic special forms: `quote`, `quasiquote`, `unquote`, `unquote-splicing`, 
  `if`, `define`, `begin`
* basic lisp list functions: `list`, `car`, `cdr`
* function construction (no variable arity yet): `lambda`
* basic timing for benchmarks: `(timed-apply <proc> <args>*)`
* tail-call optimized


To Do
-----

Many major features are yet to come:

* transition unit tests to gimmick itself
* module system
* structures
* static typing
* concurrency support
* consider possible error handling mechanisms
* native code generation
* editor integration like [SLIME][slime] for lisp in emacs
* debugging support 
* consider a 'safe' compilation/interpreter method that will not
  have functions like `set!` supported to fully enforce a non-mutable
  world view
  

Example Code
------------

A small example would look like this:

```lisp
(begin
 (define fib (lambda (n) 
    (if (< n 2) 
        1 
        (+ (fib (- n 1)) (fib (- n 2))))))
 (fib 30)        
)
```

Known Bugs
----------

* (Windows only) The [readline][chzyer] library used doesn't play nice with the terminal
  from msys2/mingw-w64, but the executable works fine from the basic command prompt.


History and Thanks
-------------------

One major influence on how this projected developed was Peter Norvig's article 
[(How to Write a (Lisp) Interpreter (in Python))][norvig1]. Long before I read that article, I stumbled on 
[Write Yourself a Scheme in 48 Hours][wyas48] back in my Haskell days. I messed around
with it and started to believe in my ability to get something of my own up and running.

In the interrim, I spent some time working with Lua having embedded it in my C graphics
library called [Portable Glue][pg]. All this time I wanted to use Scheme but things
didn't work out despite some early success with [my Gambit-C usage][am-scheme].

Now that I've been working on [Fizzle][fizzle] and my associated game libraries, I've
been looking for a good lisp-like embedded language and found quite a few. However, after
some investigation, I felt that it'd be better if I start on my own language to support
the features that I'm interested in. I came across [this article on scm.go][scmgo] which
helped restart me on the language design path. And naturally, I had to read the relevant
portions of [SICP][sicp].

License
=======

Gimmick is released under the BSD license. See the [LICENSE][license-link] file for more details.


[golang]: https://golang.org/
[chzyer]: https://github.com/chzyer/readline
[license-link]: https://raw.githubusercontent.com/tbogdala/gimmick/master/LICENSE

[norvig1]: http://norvig.com/lispy.html
[wyas48]: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
[am-scheme]: https://animal-machine.com/blog/130110_scheme_wrappers_for_a_3d_game_engine.md
[scmgo]: https://pkelchte.wordpress.com/2013/12/31/scm-go/
[sicp]: https://mitpress.mit.edu/sicp/full-text/book/book.html
[fizzle]: https://github.com/tbogdala/fizzle
[pg]: https://bitbucket.org/tbogdala/portableglue
[slime]: https://common-lisp.net/project/slime/

