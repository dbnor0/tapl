## Introduction

There are multiple approaches for unique representation of terms:

- explicit replacement of variables with "fresh" names during substitution (what does fresh mean?)

- *Barendregt convention* - all bound variables must have unique names amongst themselves & free variables. Substitution can still introduce ambiguities, which means this process must be repeated at each substitution.

- "canonical" representation that does not require renaming

- "explicit substitutions" (?)

- avoiding variables by only using combinators (i.e. combinatory logic)

## Terms and contexts

Choosing the canonical representation method, variable occurances point directly to their binders - variables are replaced by numbers, the number representing the n-th enclosing term, starting with the innermost lambda.

**For each of the following combinators, write down the corresponding nameless term.**

c0 = λs. λz. z
λ. λ. 0

c2 = λs. λz. s (s z)
λ. λ. 1 (1 0)

plus = λm. λn. λs. λz. m s (n z s)
λ. λ. λ. λ. 3 1 (2 0 1)

fix = λf. (λx. f (λy. (x x) y)) (λx. f (λy. (x x) y))
λ. (λ. 1 (λ. (1 1) 0)) (λ. 1 (λ. (1 1) 0))

foo = (λx. (λx. x)) (λx. x)
(λ. (λ. 0)) (λ. 0)

## Shifting and substitution

## Evaluation