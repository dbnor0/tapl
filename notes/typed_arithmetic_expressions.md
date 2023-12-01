## Types

We've previously defined the following values for arithmetic expressions:

```
v ::=
  true
  false
  nv

nv ::=
  0
  succ nv
```

Terms either eventually evaluate to these terms, or become stuck, meaning that no other evaluation rule applies to them.

Stuck terms indicate erroneous programs. We want to be able to tell which programs belong in this category, without necessarily running them to find out. In the context of arithmetic expressions, stuck terms are instantiated whenever a term of the wrong _'kind'_ is used in a higher-order expression (`pred false` or `if 0 then t1 else t2`).

Thus, we introduce 2 types, `Bool` and `Nat` to classify terms, and rewrite syntactic rules to take these into consideration.

Saying that a term `t` has type `T` means that we can statically show that `t` has a form characterized by `T`.

## The typing relation

The typing relation `t : T` is a set of rules assigning types to terms.

```
Types:

T ::=
  Bool
  Nat

Typing rules:

0 : Nat

true : Bool

false : Bool

 t1: Bool t2 : T t3 : T
-------------------------
if t1 then t2 else t3 : T

  t1 : Nat
-------------
succ t1 : Nat

  t1 : Nat
-------------
pred t1 : Nat

    t1 : Nat
----------------
iszero t1 : Bool
```

A term `t` is typeable if there exists a `T`, such that `t : T`

### 8.2.3

**Prove that every subterm of a well-typed term is well typed.**

0 : Nat

true : Bool

false : Bool

If `if t1 then t2 else t3 : T`, it means `t1 : Bool` is well typed, and also `t2 : T` & `t3 : T` are well typed

If `succ t : Nat` is well typed, then `t : Nat` is also well typed

If `pred t : Nat` is well typed, then `t : Nat` is also well typed

If `iszero t : Bool` is well typed, then `t : Nat` is also well typed

Similar to evaluation derivations, typing derivations witness the validity of a typing relation for a term.

In simple type systems, such as this one, each term only has one type (and thus one typing derivation). In type systems with subtyping, terms can have multiple types.

## Safety = Progress + Preservation

One of the properties of this type system is safety/soundness: well-typed terms always evaluate to a value. This can be shown in 2 steps:

 - _progress_ - a well-typed term either is a value or it can take another evaluation rule defined step
 - _preservation_ - if a well-typed term evaluates to a result, the result is also well-typed

Proof of these properties:

**Canonical forms**

If `v` is a value of type `Bool`, then `v` is of the form `true` or `false`

- our grammar defines 4 types of values, `true` and `false` immediately apply, the other 2 values do not.

If `v` is a value of type `Nat`, then `v` is of the form `0` or `succ v`

- similar to the previous reasoning

**Progress**

_We must show that if t is well typed, then t is either a value, or there exists a t', such that t -> t'_

If a term `t` is of the form `true`, `false` or `0`, then it is a value.

If its of the form `if t1 then t2 else t3 : T`, then either `t1` is a value, then, according to the canonical forms, `t` can either evaluate to `t2` or `t3`.

If `t1` evaluates to `t1'`, it means `if t1 then t2 else t3 : T` evaluates to `if t1' then t2 else t3 : T`

If its of the form `succ t1`, then either `t1` is a value, in which case `succ t1` is also a value. If its not, then `succ t1` evaluates to `succ t1'`.

If its of the form `pred t1`, then either `t1` is a value, in which case `pred t1` is also a value, by either of the 2 rules. If its not, then `pred t1` evaluates to `pred t1'`.

If its of the form `iszero t1`, then either `t1` is a value, in which case `iszero t1` is also a value. If its not, then `iszero t1` evaluates to `iszero t1'`.

**Preservation**

_We must show that if t : T and t -> t', then t' : T_

If `t` is a value, then `t` cannot be further evaluated, so the rule does not apply.

If `t` is of the form `if t1 then t2 else t3 : T`, then `t1 : Bool`, `t2 : T`, `t3 : T` and we case 3 subcases

 - `t1 -> t1'` applies; assuming all subderivations preserve the type of `t`, we can say that `t1' : Bool`, which means it has the same type as `t`

 - `t1` is `true`, which means `t` evaluates to `t2 : T`, which has the same type as `t`

 - `t1` is `false`, which means `t` evaluates to `t3 : T`, which has the same type as `t`

Proofs for terms of the form `succ t1`, `pred t1` and `iszero t1` can be similarly shown.
