## Introduction

- in lambda calculus, everything is a function

```
t ::=
  x         -- variable
  λx.t      -- abstraction
  t t       -- application
```

- parentheses are not part of the abstract syntax, but they can be used in the concrete syntax to specify precedence

- application associates to the left

- bodies of abstractions extend to the right as far as possible

- variables are *bound* within the body of an abstraction when they appear in the head of said abstraction; they are *free* when they are not

- a term with no free variables is a *combinator*

The only means through which computations can be made in lambda calculus is through applications of abstractions.

(λx. t1) t2 -> [x -> t2]t1

meaning that we're replacing all occurences of x in t1 with t2

- terms of the form (λx. t1) t2 are called **redex**es

- the reduction operation is called **beta reduction**

Evaluation strategies (defining which redex is evaluated next):

- *full beta-reduction:* any redex can be reduced

- *normal order:* the leftmost outermost redex is reduced

- *call by name:* same as normal, with the addition that redexes within abstractions are not reduced

- *call by value:* only outermost redexes whose right-hand side has already been reduced can be reduced

Evaluation strategies do not impact type systems.

## Programming in the lambda calculus

### Multiple arguments

Abstractions only have one argument by default. Functions with multiple arguments can be simulated by using *higher-order functions*, functions that return other functions themselves.

Instead of 

λx y. y x 

we have

λx. (λy. y x)

The concept is called *currying*.

### Church booleans

Boolean values can be encoded, along with a testing function, in the following manner:

tru = λt. λf. t

fls = λt. λf. f

test = λb. λx. λy. b x y

`tru` and `fls` don't inherently represent booleans, however, along with `test`, they can be correlated with the `true`/`false` values that would lead a conditional expression to evaluate the appropriate branch.

We can define logical operators as well

and = λx. y. x y fls

### 5.2.1

or = λx. y. x tru y

not = λx. x fls tru

### Pairs

### Church numerals

### Recursion

