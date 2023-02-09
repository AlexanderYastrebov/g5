# g5: An R5RS implementation in Go

I've worked on writing a few lisp-ish interpreters in the past, but have not
fully implemented a usable language.  Hence, I decided that I would implement
the entirety of R5RS, possibly moving on to SRFIs and R7RS later.

`g5` is a simple implementation of Scheme written in Go that aims to be fully
compliant with the R5RS standard.  It is currently very much a work in progress.
Still, it already has features not present in many minimal scheme
implementations, such as hygenic macros.  It even supports some minor
extensions, such as locally defined macros.

It is licensed under the BSD-0 license, effectively making it public domain.
Use it however you wish.

~ *Joshua Pritsker*
