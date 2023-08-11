## Introduction

```
t ::=
  true
  false
  if t then t else t
  0
  succ t
  pred 0
  iszero t 
```

the symbol `t` represents a metavariable - a placeholder for a term

parantheses are not part of the abstract syntax

## Syntax

**terms** (by induction) = smallest set T such that

  * { true, false, 0 } &subseteq; T
  * if t1 &isin; T, then { succ t1, pred t1, iszero t1 } &isin; T
  * if t1 &isin; T, t2 &isin; T, t3 &isin; T, then if t1 then t2 else t3 &isin; T

**terms** (by inference rules)

**true &isin; T**

**false &isin; T**
 
**0 &isin; T**

**t1 &isin; T** => **succ t1 &isin; T**

**t1 &isin; T** => **pred t1 &isin; T**

**t1 &isin; T** => **iszero t1 &isin; T**

**t1 &isin; T** **t2 &isin; T** **t3 &isin; T** => **if t1 then t2 else t3 &isin; T**

rules with no premises are called *axioms*

**terms** (set defintion)

S(0) = &empty;

S(i + 1) = { true, false, 0 } &cup; { succ t, pred t , iszero t | t &isin; S(i) } &cup; { if t1 then t2 else t3 | t1, t2, t3 &isin; S(i) }

## 3.2.4

**How many elements does S(3) have?**

|S(i + 1)| = 3 + 3 * |S(i)| + (|S(i)|)^3

|S(0)| = 0

|S(1)| = 3 + 3 * 0 + 0^3 = 3

|S(2)| = 3 + 3 * 3 + 3^3 = 39

|S(3)| = 3 + 3 * 39 + 39^3 = 59439

## 3.2.5

**Show that the sets S(i) are cumulative - for each i, we have S(i) &subseteq; S(i + 1)**

S(0) &subseteq; S(1)

Suppose S(i) &subseteq; S(i + 1) is true

We need to show that S(i + 1) &subseteq; S(i + 2) is also true. To reformulate, we need to show that &forall; s &isin; S(i + 1), s &isin; S(i + 2) as well.

The case for the constants is self explanatory.

For the inductive terms, If s &isin; S(i + 1), by definition s &isin; S(i + 2) as well, since the inductive terms of S(i + 2) are built by applying the constructors over all terms of S(i + 1).

## Induction on terms

The inductive definition of the grammar allows us to give inductive definitions of functions over the set & to give inductive proofs of properties of the set.

## 3.3.1

[Implemented here](..\app\Exercise\UntypedArithmeticExpressions.hs)

## 3.3.2

[Implemented here](..\app\Exercise\UntypedArithmeticExpressions.hs)

## 3.3.3

Show that size(t) <= |consts(t)|

Suppose size(t') <= |consts(t')|, where t' &subset; t

|consts(true)| = size(true) = 1 ✅

|consts(false)| = size(false) = 1 ✅

|consts(0)| = size(0) = 1 ✅

|consts(succ(t))| <= size(succ(t)) 

|consts(t')| <= size(t') + 1 ✅

|consts(pred(t))| <= size(pred(t)) 

|consts(t')| <= size(t') + 1 ✅

|consts(iszero(t))| <= size(iszero(t)) 

|consts(t')| <= size(t') + 1 ✅

|consts(if t1 then t2 else t3)| <= size(if t1 then t2 else t3)

|consts(t1')| + |consts(t2')| + |consts(t3')| <= size(t1') + size(t2') + size(t3') + 1 ✅

## Semantic styles


