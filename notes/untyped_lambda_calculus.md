## Introduction

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

---

(λx. t1) t2 -> [x -> t2]t1

meaning that we're replacing all occurences of x in t1 with t2

- terms of the form (λx. t1) t2 are called **redex**es

- the reduction operation is called **beta reduction**

---

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

---

### Church booleans

Boolean values can be encoded, along with a testing function, in the following manner:

tru = λt. λf. t

fls = λt. λf. f

test = λb. λx. λy. b x y

`tru` and `fls` don't inherently represent booleans, however, along with `test`, they can be correlated with the `true`/`false` values that would lead a conditional expression to evaluate the appropriate branch.

We can define logical operators as well

and = λx. λy. x y fls

---

**Define logical or and not functions**

or = λx. λy. x tru y

not = λx. x fls tru

---

### Pairs

Pairs can be encoded by always passing a selector function that, when applied to one, will return either the first or second element of the tuple.

pair = λx. λy. λs. s x y

fst = λp. p tru

snd = λp. p fls

The selector functions are actually just booleans that select the corresponding element.

---

### Church numerals

Natural numbers can be represented by taking 2 functions - `s` representing a successor function, and `z` representing the zero constant:

c0 = λs. λz. z

c1 = λs. λz. s z

c2 = λs. λz. s (s z)

...

Each increment represents an additional application of the successor function on the previous result.

scc = λn. λs. λz. s (n s z)

---

**Find another way to define the successor function on Church numerals**

scc' = λn. λs. λz. n s (s z)

Instead of applying another succession function at the end, we simply increment the 'zero constant' that we start the expression with.

---

Addition can be defined in a similar manner in the sense that, given 2 Church numerals, we can define their sum as having one numeral's zero constant replaced with the other numeral:

plus = λm. λn. λs. λz. m s (n s z)

```haskell
-- 2 + 3
plus (λs. λz. s s z) (λs. λz. s s s z)

(λm. λn. λs. λz. m s (n s z)) (λs. λz. s s z) (λs. λz. s s s z)

(λn. λs. λz. (λs. λz. s s z) s (n s z)) (λs. λz. s s s z)

(λs. λz. (λs. λz. s s z) s ((λs. λz. s s s z) s z))

(λs. λz. (λz. s s z) ((λs. λz. s s s z) s z))

λs. λz. (s s ((λs. λz. s s s z) s z))

λs. λz. (s s (λz. s s s z) z)

λs. λz. (s s s s s z)
```

---

times = λm. λn. m (plus n) c0

This function is defined a little differently from the rest. It abstracts over the successor and zero arguments we've been directly working it, and instead leverages the functionality of Church numerals to apply `plus n` m times to the Church representation of 0.

---

**Is it possible to define multiplication on Church numerals without using plus?**

times = λm. λn. λs. λz. m (n s) z

---

**Define a term for raising one number to the power of another**

pow = λm. λn. n (times m) c1

---

Determining whether a numeral is 0 can be done by returning `tru` if the successor function is not being applied at all, and `fls` otherwise. We can do this by substituting the zero constant with `tru` and the successor function with an abstraction that always returns `fls` when applied to a numeral:

iszro = λm. m (λx. fls) tru

---

We can define a predecessor function by introducing a special kind of successor function that works with pairs - when incrementing a pair, only the second value is actually incremented, thus, the predecessor of the second element is the first element of the tuple.

zz = pair c0 c0

ss = λp. pair (snd p) (scc (snd p))

prd = λm. fst (m ss zz)

---

**Use `prd` to define a subtraction function**

minus = λm. λn. n prd m

---

**Approximately how many steps of evaluation (as a function of n) are required to calculate `prd cn`?**

```haskell
(λm. fst (m ss zz)) (λs. λz. z)                  
fst ((λs. λz. z) ss zz)                                                                                 -- β
fst ((λz. z) zz)                                                                                        -- β
fst (zz)                                                                                                -- β
fst (pair (λs. λz. z) (λs. λz. z))
fst ((λx. λy. λs. s x y) (λs. λz. z) (λs. λz. z))
fst ((λy. λs. s (λs. λz. z) y) (λs. λz. z))                                                             -- β
fst (λs. s (λs. λz. z) (λs. λz. z))                                                                     -- β
(λp. p tru) (λs. s (λs. λz. z) (λs. λz. z))
(λs. s (λs. λz. z) (λs. λz. z)) tru                                                                     -- β
(λs. s (λs. λz. z) (λs. λz. z)) (λx. λy. x)
(λx. λy. x) (λs. λz. z) (λs. λz. z)                                                                     -- β
(λy. (λs. λz. z)) (λs. λz. z)                                                                           -- β
(λs. λz. z)                                                                                             -- β
```

steps(c0) = 9

```haskell
(λm. fst (m ss zz)) (λs. λz. s z)
fst ((λs. λz. s z) ss zz)                                                                               -- β
fst ((λz. ss z) zz)                                                                                     -- β
fst (ss zz)                                                                                             -- β
fst (λp. pair (snd p) (scc (snd p))) (pair (λs. λz. z) (λs. λz. z))
fst (pair (snd (pair (λs. λz. z) (λs. λz. z))) (scc (snd (pair (λs. λz. z) (λs. λz. z)))))              -- β
fst (pair (snd (λx. λy. λs. s x y) (λs. λz. z) (λs. λz. z)) (scc (snd (pair (λs. λz. z) (λs. λz. z)))))
fst (pair (snd (λy. λs. s (λs. λz. z) y) (λs. λz. z)) (scc (snd (pair (λs. λz. z) (λs. λz. z)))))       -- β
fst (pair (snd (λs. s (λs. λz. z) (λs. λz. z))) (scc (snd (pair (λs. λz. z) (λs. λz. z)))))             -- β
fst (pair ((λp. p fls) (λs. s (λs. λz. z) (λs. λz. z))) (scc (snd (pair (λs. λz. z) (λs. λz. z)))))
fst (pair ((λs. s (λs. λz. z) (λs. λz. z)) fls) (scc (snd (pair (λs. λz. z) (λs. λz. z)))))             -- β
fst (pair (fls (λs. λz. z) (λs. λz. z)) (scc (snd (pair (λs. λz. z) (λs. λz. z)))))                     -- β
fst (pair ((λx. λy. y) (λs. λz. z) (λs. λz. z)) (scc (snd (pair (λs. λz. z) (λs. λz. z)))))
fst (pair ((λy. y) (λs. λz. z)) (scc (snd (pair (λs. λz. z) (λs. λz. z)))))                             -- β
fst (pair (λs. λz. z) (scc (snd (pair (λs. λz. z) (λs. λz. z)))))                                       -- β
fst (pair (λs. λz. z) (scc (snd ((λx. λy. λs. s x y) (λs. λz. z) (λs. λz. z)))))
fst (pair (λs. λz. z) (scc (snd ((λy. λs. s (λs. λz. z) y) (λs. λz. z)))))                              -- β
fst (pair (λs. λz. z) (scc (snd (λs. s (λs. λz. z) (λs. λz. z)))))                                      -- β
fst (pair (λs. λz. z) (scc ((λp. p fls) (λs. s (λs. λz. z) (λs. λz. z)))))
fst (pair (λs. λz. z) (scc ((λs. s (λs. λz. z) (λs. λz. z)) fls)))                                      -- β
fst (pair (λs. λz. z) (scc ((λs. s (λs. λz. z) (λs. λz. z)) (λx. λy. y))))
fst (pair (λs. λz. z) (scc (((λx. λy. y) (λs. λz. z) (λs. λz. z)))))                                    -- β
fst (pair (λs. λz. z) (scc (((λy. y) (λs. λz. z)))))                                                    -- β
fst (pair (λs. λz. z) (scc ((λs. λz. z))))                                                              -- β
fst (pair (λs. λz. z) ((λn. λs. λz. s (n s z)) ((λs. λz. z))))
fst (pair (λs. λz. z) ((λs. λz. s (((λs. λz. z)) s z))))                                                -- β
fst (pair (λs. λz. z) ((λs. λz. s (((λz. z)) z))))                                                      -- β
fst (pair (λs. λz. z) ((λs. λz. s z)))                                                                  -- β
fst ((λx. λy. λs. s x y) (λs. λz. z) ((λs. λz. s z)))
fst ((λy. λs. s (λs. λz. z) y) ((λs. λz. s z)))                                                         -- β
fst (λs. s (λs. λz. z) (λs. λz. s z))                                                                   -- β
(λp. p tru) (λs. s (λs. λz. z) (λs. λz. s z))
(λs. s (λs. λz. z) (λs. λz. s z)) tru                                                                   -- β
(λs. s (λs. λz. z) (λs. λz. s z)) (λx. λy. x)
((λx. λy. x) (λs. λz. z) (λs. λz. s z))                                                                 -- β
((λy. (λs. λz. z)) (λs. λz. s z))                                                                       -- β
(λs. λz. z)                                                                                             -- β
```

steps(c1) = 25

We haven't used a well-defined evaluation strategy, however, we can assume that

steps(cn) ~= 16n

given this strategy.

---

**Write a function equal that tests two numbers for equality and returns a Church boolean**

equal = λm. λn. iszro (minus m n)

---

**A list can be represented in the lambdacalculus by its fold function. For example, the list [x,y,z] becomes a function that takes two arguments c and n and returns c x (c y (c z n)). What would the representation of `nil` be?**

nil = λc. λn. n

**Write a function `cons` that takes an element h and a list (that is, a fold function) t and returns a similar representation of the list formed by prepending h to t.**

cons = λh. λl. λc. λn. c h (l c n)

**Write `isnil` and `head` functions, each taking a list parameter.**

isnil = λl. l (λx. λy. fls) tru

`isnil` is similar to `iszro`, except we need to ignore both the `cons` function and its value when returning `fls`.

head = λl. l tru nothing

We're introducing a special `nothing` value to return in case the list is empty, otherwise, we'll return the first variable between the head and the tail of the list, namely the head.

**Finally, write a tail function for this representation of lists**

### Recursion

