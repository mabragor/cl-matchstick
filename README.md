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

Also, as of now, the code is poorly tested, but I hope this will quickly improve.

We have three matching macros: WITH-MATCH, WHEN-MATCH and ECASE-MATCH.
WITH-MATCH throws FAIL-MATCH error, if match failed, WHEN-MATCH returns NIL and doesn't execute
its body, ECASE-MATCH consists of a number of specs -- it tries to match each spec in turn and
throws FAIL-MATCH only if all matches failed.

Again, for examples of usage see CL-VHDL, emitting branch (and hopefully soon master branch as well).
