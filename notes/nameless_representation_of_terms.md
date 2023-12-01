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

We using naming contexts, Γ, to be able to handle both bound & free variables. Captured variables are handled by the implicit canonical representation, however we also need to handle free variables.

Captured variables are handled first (0, 1, .., n), followed by free variables (n + 1, n + 2, ..)

Given the order of entries in the naming contexts determines their index, we can express Γ as a sequence

## Shifting and substitution

Substitution implies reindexing of variables within a context, since we're introducing a new bound variable.

[1 -> s](λ. 2)

Since we're introducing the free variables of s in the context of an abstraction, the bound variable of the abstraction will take priority over the free variables with regards to indices, so all free variables of s need to be shifted up by 1.

Indices of free variables follow after the ones of bound variables, which means we can define a cutoff c above which variables need to be shifted.

```
↑(d, c) k       = k    , k < c
                = k + d, k >= c
↑(d, c) (λ. t)  = λ. ↑(d, c + 1) t
↑(d, c) (t1 t2) = (↑(d, c) t1) (↑(d, c) t2)
```

↑(d) refers to ↑(d, 0)

### 6.2.2

↑(2) (λ.λ. 1 (0 2)) = (λ.λ. 1 (0 4))

↑2 (λ. 0 1 (λ. 0 1 2)) = (λ. 0 3 (λ. 0 1 4))

With shifting in place, substitution can now also be defined, in a similar manner to what we previously had, with the exception of abstractions, where we'll be using shifting:

```
[j -> s]k = s, j =  k
[j -> s]k = k, j != k
[j -> s]λ. t = λ. [j + 1 -> ↑(1)s]t
[j -> s]t1 t2 = ([j -> s]t1 [j -> s]t2)d
```

We increment `j` and shift up the free variables of `s` for each abstraction we encounter. Since each abstraction introduces a new bound variables which in turn shifts all existing indices up by one, we need to do the same with our indices.

## Evaluation

Only rule that changes is substitution:
  - as previously noted, we need to shift the term we're currently substituting for by 1
  - we need to shift the whole result of the substitution down by 1, to accomodate for using up the bound variable

This yields:

`(λx. t)v -> ↑(-1)([0 -> ↑(1)v]t)`