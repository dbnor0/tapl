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

### 3.2.4

**How many elements does S(3) have?**

|S(i + 1)| = 3 + 3 * |S(i)| + (|S(i)|)^3

|S(0)| = 0

|S(1)| = 3 + 3 * 0 + 0^3 = 3

|S(2)| = 3 + 3 * 3 + 3^3 = 39

|S(3)| = 3 + 3 * 39 + 39^3 = 59439

### 3.2.5

**Show that the sets S(i) are cumulative - for each i, we have S(i) &subseteq; S(i + 1)**

S(0) &subseteq; S(1)

Suppose S(i) &subseteq; S(i + 1) is true

We need to show that S(i + 1) &subseteq; S(i + 2) is also true. To reformulate, we need to show that &forall; s &isin; S(i + 1), s &isin; S(i + 2) as well.

The case for the constants is self explanatory.

For the inductive terms, If s &isin; S(i + 1), by definition s &isin; S(i + 2) as well, since the inductive terms of S(i + 2) are built by applying the constructors over all terms of S(i + 1).

## Induction on terms

The inductive definition of the grammar allows us to give inductive definitions of functions over the set & to give inductive proofs of properties of the set.

### 3.3.1

[Implemented here](..\app\Exercise\UntypedArithmeticExpressions.hs)

### 3.3.2

[Implemented here](..\app\Exercise\UntypedArithmeticExpressions.hs)

### 3.3.3

Show that size(t) <= |consts(t)|

Suppose side(t') <= |consts(t')|, where t' &subset; t

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

**Operational semantics** = assigning meaning to a program by specifynig an abstract (not actual low level bytecode) machine to execute it. In this context, the meaning of the term is the value it has at the end of the so-called execution, when starting with a given initial value.

*Small-step style* involves only one transition in the context of execution, while *big-step style* means that transitions are evaluated to their final result.

**Denotational semantics** = correlating a program with a mathematical object. It abstracts low-level implementation details of languages and focuses on high-level concepts. Given the mathematical nature of the specification, it becomes easier to derive or prove certain things about programs.

**Axiomatic semantics** = "Axiomatic
methods take the laws themselves as the definition of the language.
The meaning of a term is just what can be proved about it"


## Evaluation

![Boolean expressions definition](assets\img\boolean_expressions.png)

*Evaluation rules determine a particular strategy/order for terms to be evaluated in*

**E-IfTrue** and **E-IfFalse** are computation rules

**E-If** is a congruence rule

An *instance* of an inference rule is obtained by replacing metavariables with the same term in the rule's conclusion/premise.

if true then true else (if false then false else false) -> true

is an instance of **E-IfTrue**

A rule is *satisfied* by a relation if, for each instance of the rule, either the conclusion is in the relation or one of the premises is not. (similar to logical implication)

**One-step evaluation relation** = smallest binery relation -> on terms, such that all inference rules are satisfied.

For boolean expressions, the derivability of a statement can be shown using a derivation tree, where the root is the statement for which we're trying to show a derivation path, internal nodes are applications of E-If and leaves are applications of E-IfTrue or E-IfFalse.

Since our syntax only specifies one rule that has a premise, derivability trees are in fact always going to be linked lists. Branching can happen when a rule has multiple premises.

Derivation trees are useful for proving properties about derivation statements.

### 3.5.4

**If t -> t' and t -> t'' then t' = t''**

If t' is of the form

if t1 then t2 else t3

then t1 is true. This means that t'' cannot be

if false then t2 else t3

t'' cannot be an instance of E-If either, since true cannot be further evaluated to anything. In conclusion, t' = t''.

The same applies for the case in which t1 is false.

If t' is of the form

if t1 then t2 else t3

where t1 -> t0, then t1 can't be true or false, since there are no rules for further evaluation for constants, which means t'' must be an instance of E-If as well.

### 3.5.5

If, for each term t1, t1' and t1'', we can show that if t1 -> t1' and t1 -> t1'',t1'= t1'' for t1 < t2, t1' < t2' and t1'' < t3'', then we can show that if t2 -> t2' and t2 -> t2'', then t2' = t2'' for all t2, t2', t2''.

A term t is in **normal form** if no evaluation rule applies to it.

Every value is in normal form.

**Multi-step evaluation relation** = reflexive, transitive closure of one-step evaluation:

if t -> t' then t ->* t'

&forall; t, t ->* t

if t ->* t' and t' ->* t'' then t -* t''

### 3.5.13

Suppose we add the rule if true then t2 else t3 -> t3

* a. t -> t' and t -> t'' implies t' = t'' no longer holds because if true then .. now has 2 divergent? valid rules that apply to it

* b. every value would still be in normal form since we're not introducing any new values and we're not introducing transitions from values either

* c. every normal form is still a value for the same reasons as in b

* d. t ->* t' and t ->* t'' implies t' = t'' no longer holds either for the same reasons as in a

* e. for every term t there is a normal form t' such that t ->* t' still holds for the same reasons as in b

Suppose we add the rule if t2 -> t2' then if t1 then t2 else t3 -> if t1 then t2' else t3

* a. t -> t' and t -> t'' implies t' = t'' no longer holds because if true then .. now has 2 valid rules that apply to it

* b. every value would still be in normal form since we're not introducing any new values and we're not introducing transitions from values either

* c. every normal form is still a value for the same reasons as in b

* d. t ->* t' and t ->* t'' implies t' = t'' holds because while we have introduced a new rule, it just specifies a different evaluation order for a certain term. When applying all possible evaluation rules to it, it will eventually lead to the same normal form as before

* e. for every term t there is a normal form t' such that t ->* t' still holds for the same reasons as in b

![Arithmetic expressions definition](assets\img\arithmetic_expressions.png)

The new group `nv` is introduced to denote the specific types of values arithmetic expressions can evaluate to.

### 3.5.14

**Show that if t -> t' and t -> t'' then t' = t''**

succ t1 -> succ t1' when t1 -> t' is the only evaluation rule that concerns succ, so by definition t' = t''.

If t is pred t1 -> pred t1' when t1 -> t1', that implies that t1 can be further evaluated. The other rules that involve pred - E-PredZero and E-PredSucc all match usages of pred having a value argument, which are in normal form, so t' = t''.

If t is pred 0, then succ nv cannot be 0, nor can it be a term that can be further evaluated (same applies the other way around), so t' = t''.

If t is iszero t1 -> iszero t1' when t1 -> t1', that implies that t1 can be further evaluated. The other rules that involve iszero - E-IsZeroZero and E-IsZeroSucc all match usages of iszero having a value argument, which are in normal form, so t' = t''.

If t is iszero 0, then succ nv cannot be 0, nor can it be a term that can be further evaluated (same applies the other way around), so t' = t''.

### 3.5.16

```
badnat ::=
  wrong
  true
  false
```

```
badbool ::=
  wrong
  nv
```

if badbool then t1 else t2 -> wrong

succ badnat -> wrong

pred badnat -> wrong

iszero badnat -> wrong

Stuck terms in the original grammar are non-value terms that cannot be further evaluated. We can identify these terms as

if nv then t1 else t2

succ bv

pred bv

iszero bv

The extension for the grammar introduces rules that lead to these terms being evaluated to `wrong`. Similarly, if one of the extension rules are to be evaluated, we can clearly see that the terms in the original grammar would be in a stuck state, so the two ways of handling stuck values is equivalent.

3.5.17

todo

3.5.18

todo
