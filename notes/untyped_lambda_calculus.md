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

head = λl. l tru nil

**Finally, write a tail function for this representation of lists**

todo

### Enriching the calculus

Working with actual primitives is sometimes beneficial over using Church encodings.
Conversions can be defined between the 2 forms.

### Recursion

The equivalent of the Y combinator in Haskell is `fix`

```haskell
fix :: (a -> a) -> a
fix f = f (fix f)
```

This will infinitely apply a function to itself. Its most useful when applying it to functions that take a 'recurse' function as a parameter and have a base case which does not call it. Despite the function infinitely calling itself _in a vacuum_, due to Haskell's lazy evaluation, the construct will only recurse as far as needed.

Take the factorial function, which can be defined as

`fix (\fact n -> if n == 1 then 1 else n * fact (n - 1))`

```haskell
fix (\fact n -> if n == 1 then 1 else n * fact (n - 1)) 3

(\fact n -> if n == 1 then 1 else n * fact (n - 1)) fix (\fact n -> if n == 1 then 1 else n * fact (n - 1)) 3

n -> if n == 1 then 1 else n * (fix (\fact n -> if n == 1 then 1 else n * fact (n - 1))) (n - 1) 3

if 3 == 1 then 1 else 3 * (fix (\fact n -> if n == 1 then 1 else n * fact (n - 1))) 2

3 * (fix (\fact n -> if n == 1 then 1 else n * fact (n - 1))) 2

3 * (\fact n -> if n == 1 then 1 else n * fact (n - 1)) (fix (\fact n -> if n == 1 then 1 else n * fact (n - 1))) 2

3 * n -> if n == 1 then 1 else n * (fix (\fact n -> if n == 1 then 1 else n * fact (n - 1))) (n - 1) 2

3 * if 2 == 1 then 1 else 2 * (fix (\fact n -> if n == 1 then 1 else n * fact (n - 1))) 1

3 * 2 * (fix (\fact n -> if n == 1 then 1 else n * fact (n - 1))) 1

3 * 2 * (\fact n -> if n == 1 then 1 else n * fact (n - 1)) (fix (\fact n -> if n == 1 then 1 else n * fact (n - 1))) 1

3 * 2 * n -> if n == 1 then 1 else n * (fix (\fact n -> if n == 1 then 1 else n * fact (n - 1))) 1

3 * 2 * if 1 == 1 then 1 else 1 * (fix (\fact n -> if n == 1 then 1 else n * fact (n - 1)))

3 * 2 * 1
```

In eager languages, the presence of `fix` would lead to infinite evaluation, but since Haskell is lazy, each application of `fact` will be evaluated before the construct recurses, allowing it to stop whenever the `n == 1` base case is reached.

todo: add something about the Z combiantor defined in the book.

**Why did we use a primitive if in the definition of g, instead of the Church-boolean test function on Church booleans? Show how to define the factorial function in terms of test rather than if.**

Because `realeq` returns real booleans. If we want to use `test`, we should use the `equal` function defined previously:

`g = λfct. λn. test (equal n c0) c1 (times n (fct (prd n)))`

**Define a function churchnat that converts a primitive natural number into the corresponding Church numeral**

`g = λfct. λn. if (iszero n) then z else s (fct (pred n))`

`churchnat = λn. λs. λz. (fix g) n`

**Use fix and the encoding of lists to write a function that sums lists of Church numerals**

Suppose `isnil` returns real booleans:

`g = λfct. λl. if isnil l then c0 else plus (head l) (fct (tail l))`

`sum = λl. λc. λn. (fix g) l`

### Formalities

#### Syntax

V - set of countable variable names

T - set of terms

1. &forall; x &isin; V then x &isin; T

2. if t1 &isin; T and x &isin; V, then λx. t1 &isin; T

3. if t1, t2 &isin; T, then t1 t2 &isin; T

FV - set of free variables

FV(x) = { x }

FV(λx. t1) = FV(t1) \ { x }

FV(t1 t2) = FV(t1) &cup; FV(t2)

#### Substitution

Substitution is generally straight-forward - matching free variables get replaced by their substitutes. The problem arises when working with abstractions & bound variables. We don't want to substitute bound variables for one, and we don't want to substitute free variables *for* bound variables either.

Since the names of bound variables do not matter (meaning that they can be renamed), we can treat substitution as a total function:

[x -> s]x = s

[x -> s]y = y, when x != y

[x -> s]λy. t = λy. [x -> s]t, when x != y and y &notin; FV(s)

[x -> s]t1 t2 = [x -> s]t1 [x -> s]t2

**Call by value: only outermost redexes whose right-hand side has already been reduced can be reduced**

id (id (λz. id z))     | #2
id (λz. id z)          | #3
λz. id z

Syntax
```
t ::=
  x
  λx. t
  t t

v ::=
  λx. t
```

Evaluation
```
   t1 -> t1'
---------------
t1 t2 -> t1' t2

   t2 -> t2'
---------------
v1 t2 -> v1 t2'

(λx. t12) v2 -> [x -> v2]t12
```

**Full beta-reduction: any redex can be reduced**

Syntax
```
t ::=
  x
  λx. t
  t t

v ::=                                            
  λx. t
```

Evaluation
```
   t1 -> t1'
---------------
t1 t2 -> t1' t2

   t2 -> t2'
---------------
t1 t2 -> t1 t2'

      t -> t'
-------------------
(λx. t) -> (λx. t')

(λx. t12) t2 -> [x -> t2]t12
```

**Normal order: the leftmost outermost redex is reduced**

Syntax
```
t ::=
  x
  λx. t
  t t

v ::=                                            
  λx. t
```

Evaluation
```
   t1 -> t1'
---------------
t1 t2 -> t1' t2

      t -> t'
-------------------
(λx. t) -> (λx. t')

(λx. t12) t2 -> [x -> t2]t12
```

**Call by name: same as normal, with the addition that redexes within abstractions are not reduced**

id (id (λz. id z))     | #2
id (λz. id z)          | #2
λz. id z

Syntax
```
t ::=
  x
  λx. t
  t t

v ::=                                            
  λx. t
```

Evaluation
```
   t1 -> t1'
---------------
t1 t2 -> t1' t2

(λx. t12) t2 -> [x -> t2]t12
```
  

**Previously, an alternative presentation of the operational
semantics of booleans and arithmetic expressions was given in which stuck
terms are defined to evaluate to a special constant wrong. Extend this semantics
to λNB**

todo

**Previously, a "big-step" style of evaluation for
arithmetic expressions was introduced, where the basic evaluation relation is "term t evaluates
to final result v." Show how to formulate the evaluation rules for lambdaterms
in the big-step style**