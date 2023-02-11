# g5: An R5RS implementation in Go

I've worked on writing a few lisp-ish interpreters in the past, but have not
fully implemented a usable language.  Hence, I decided that I would implement
the entirety of R5RS, possibly moving on to SRFIs and R7RS later.

`g5` is a simple implementation of Scheme written in Go that aims to be fully
compliant with the R5RS standard.  It is currently still missing some things,
but is functional.  It supports features often excluded from minimal scheme
implementations, such as proper R5RS macros.  It also supports a few extensions
and SRFIs.

It is licensed under the BSD-0 license, effectively making it public domain.
Use it however you wish.

~ *Joshua Pritsker*
