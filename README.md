cl-matchstick
-------------

Yet another pattern-matching library for CL. This one focuses on most conveniently parsing s-exps.
It's extensively used in CL-VHDL (hence, look there for examples of usage, `emitting` branch).

It's kinda like DESTRUCTURING-BIND on steroids.

;; simplest case
```lisp
(with-match (a b c) '(1 2 3)
  ;; here A B C are bound to 1 2 3
  )
```

;; instead of read-time dot, here we have CDR special form
```lisp
(with-match (a (cdr b)) '(1 . 2)
  ;; here A B are bound to 1 2
  )
```

We also have other special forms:
  * (OR &rest subpatterns) -- matches alternative sub-patterns
  * (LEN n subpattern) -- matches list of specified length, then destructures it using subpattern
  * (NOT subpattern) -- succeeds, whenever subpattern fails
  * (MAYBE subpattern) -- either successfully destructures the subpattern, or yields NIL, in which
    case "cursor" is not advanced further along the list
  * (CAR subpattern) -- like CDR, but uses CAR stepping function (instead of default CADR)
  * (COLLECT-WHILE subpattern) -- greedily consume elements of the list, as long as they match the pattern
  * (COLLECT-UNTIL subpattern) -- shortcut to (COLLECT-WHILE (NOT subpattern))
  * (CAP name subpattern) -- "capture" value of subpattern in a name (becomes visible inside the body)

So far that's all special forms we have -- maybe more will be added once I figure out I need them
for CL-VHDL project destructuring.

Only a handful of atoms are supported in patterns now:
  * if something is a keyword, it's matched using EQ
  * if something is a symbol (but not a keyword) the corresponding symbol is bound
    (using SYMBOL-MACROLET) inside the body of the macro
  * additionally, if symbol-name is of the form name_predicate (i.e. contains underscore)
    it's interpreted as a predicate test: subexpression should satisfy PREDICATE and if it
    does, its value is bound to NAME inside body of the macro
    (this notation is the same as in Wolfram Mathematica)

Also, as of now, the code is poorly tested, but I hope this will quickly improve.

We have following matching macros:
  * WITH-MATCH pattern thing &body body -- tries to match THING against PATTERN, if it succeeds,
    body is executed with variables inside pattern bound to parts of expression. If it fails,
    FAIL-MATCH error is thrown
  * WHEN-MATCH pattern thing &body body -- like WITH-MATCH, but on failure quietly returns NIL,
    without throwing anything
  * ECASE-MATCH thing &rest specs -- like repeated WITH-MATCH. First element of each element of SPECS
    is a pattern, that tries to be matched against. On success, the corresponding "body" is executed.
    On failure, subsequent clauses are tried. If none matches, FAIL-MATCH is thrown.
  * MATCH-P pattern thing -- simply returns T if THING matches PATTERN, NIL otherwise.

Again, for examples of usage see CL-VHDL, emitting branch (and hopefully soon master branch as well).
