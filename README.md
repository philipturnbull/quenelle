Quenelle
========

A basic semantic patching tool for Python 2 code, similar to [Coccinelle](http://coccinelle.lip6.fr/). Semantic patching allows generic patches to be written which ignore incidental formatting, such as whitespace:

    $ cat examples/lambda.rule
    replace:
    map(lambda V1: E1(V1), E2)
    with:
    map(E1, E2)
    ~/code/quenelle
    $ ./quenelle examples/lambda.rule examples/lambda.py
    ====[ examples/lambda.py:8 ]================================
    SREBMUN = map(lambda x: reverse_upper(x), numbers.split())
    ============================================================
      - map(lambda x: reverse_upper(x), numbers.split())
      + map(reverse_upper, numbers.split())
    
    
    ====[ examples/lambda.py:9 ]================================
    srebmun = map(lambda y: reverse_lower(y), reversed(numbers.split()))
    ============================================================
      - map(lambda y: reverse_lower(y), reversed(numbers.split()))
      + map(reverse_lower, reversed(numbers.split()))

Building
--------

Build in a cabal sandbox:

    cabal sandbox init
    cabal install --enable-tests --only-dependencies
    cabal build && cabal test

Testing is a mixture of unit-tests using HUnit and QuickCheck tests. Example rule and source files are contained in the `examples` directory.

Example
-------

A Quenelle rule consists of a `replace` stanza followed by a `with` stanza. Stanzas support all Python 2 syntax that is supported by language-python. Expressions and variables can be bound in the `replace` stanza and used in the `with` stanza using the `E<n>` and `V<n>` syntax respectively. If an expression or variable is not needed in the `with` stanza it can be bound with `E` or `V` instead.

    $ cat examples/filter.rule
    replace:
    [E1 for E1 in E2 if E3(E1)]
    with:
    filter(E3, E2)
    $ ./quenelle examples/filter.rule examples/filter.py
    ====[ examples/filter.py:2 ]================================
    ys = [x for x in xs if len(x)]
    ============================================================
      - [x for x in xs if len(x)]
      + filter(len, xs)

This rule captures three expressions, `E1`, `E2` and `E3`. All instances of a bound expression must be equal, so this rule would not match `[x+1 for x in xs if f(x)]` because not all occurrences of `E1` do not match.

Numeric literals match based on their value, so a `f(E1 + 0)` stanza will match `f(10 + 0x00000000)`.

Limitations
-----------

Quenelle only currently supports matching and replacing expressions. Statements cannot be transformed.
