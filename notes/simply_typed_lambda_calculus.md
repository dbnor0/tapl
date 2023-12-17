## Function types

The goal is to assign types to the previously untyped calculus (variables, abstractions and applications), with `Bool` as a valid type for variables. We're especially interested in finding a type for abstractions. Using a singular type `->` for all functions would be too conservative, since both

`λx.true`

and

`λx.λy.y`

would be assigned the same type, despite having different arities and return types. Since we're interested in both the argument and return types, functions should be parametrized in these values - `T -> T`, leading to the following set of types

```
T ::=
  Bool
  T -> T
```

Functions associate to the right - `T -> T -> T` is `T -> (T -> T)`

## The typing relation

We need to establish a way to assign a type to the arguments of abstractions. This can either by done by using an explicit annotation

`λx:T. t`

or by analyzing the body of the abstraction to see how the argument is used, and infer a type based on that. Languages that use choice #1 are _explicitly typed_, and languages that use choice #2 are _implicitly typed_. The return type of the abstraction is straight-forward: it is the type of `t`. Occurances of `x` in `t` denote `T` as a type. This can be standardized as

```
    x:T1 |- t:T2
---------------------
|- λx:T. t : T1 -> T2
```

Since abstraction may be nested, we need a medium through which to keep track of multiple bound/free variables. We're use typing contexts (`Γ`) for this purpose, formalized as mappings from variable names to their types, and we're explicitly making them part of subsequent rules.

Names chosen for newly introduced variables are assumed to be unique in the context to which they're being added. In addition, contexts can be extended using the comma operator. Thus, the previous rule now becomes

```
    Γ, x:T1 |- t:T2
-----------------------
Γ |- λx:T. t : T1 -> T2
```

The rule for variables is can be expressed similarly:

```
x:T ∈ Γ
---------
Γ |- x:T
```

For a mapping beloning to a context, the type of the variable is the one indicated in the mapping.

Finally, applications of terms to functions have the following typing rule:

```
Γ |- t1 : T11->T12 Γ |- t2 : T11
--------------------------------
        Γ |- t1 t2 : T12
```

### 9.2.1
**The pure simply typed lambda-calculus with no base types is actually degenerate, in the sense that it has no well-typed terms at all. Why?**

For a term to be well-typed, we need to be able to assign it a type. The only types available in pure lambda-calculus are function types, however, there are no concrete argument and return types we can assign to terms as a base case.

Rules for the simply-typed, pure lambda calculus

```
Syntax
t ::=
  x
  λx:T. t
  t1 t2

v ::=
  λx:T. t

T ::=
  T -> T

Γ ::=
  ∅
  Γ, x:T

Evaluation

   t1 -> t1'
---------------
t1 t2 -> t1' t2

   t2 -> t2'
---------------
v1 t2 -> v1 v2

(λx:T11. t12) v2 -> [x -> v2]t12

Typing

x:T ∈ Γ
---------
Γ |- x:T

    Γ, x:T1 |- t:T2
-----------------------
Γ |- λx:T. t : T1 -> T2

Γ |- t1 : T11->T12 Γ |- t2 : T11
--------------------------------
        Γ |- t1 t2 : T12
```

## Properties of typing

todo

## The Curry-Howard correspondence

`->` is like a type constructor that takes 2 other types and returns a result type

The abstraction or _introduction_ rule tells us how `->` types are constructed

The application or _elimination_ rule tells us how `->` types are used

Whenever abstraction is an immediate subterm of application, we basically have computation

The introduction/elimination analogy stems from the link between type theory and logic, known as the Curry-Howard correspondence. It stipulates that propositions and types are equivalent, and that the mappings between entities in both domains are isomorphic. Proofs of propositions correspond to programs. 

The equivalence can be defined as follows:

```
propositions                 types
proposition P -> Q           type P -> Q
proposition P ^ Q            type P x Q
proof of proposition P       a term t of type P
proposition P is provable    type P is inhabited by at least one term
```

## Erasure and typeability

Despite not being used, we still mentioned type annotations in the evaluation rules. Most compilers however do not carry type annotations at runtime, they are only used during type-checking and code generation.

Erasure of simply typed terms can be defined as

```
erase(x) = x
erase(λx:T1. t2) = λx. erase(t2)
erase(t1 t2) = erase(t1) erase(t2)
```

It doesn't matter when we do erasure (before or after evaluation), we should still get the same result.

## Curry-style vs Church-style

Curry-style (extrinsic types) is semantics prior to typing. We first define evaluation rules for terms, and the type system is not a part of this. 

Church-style (intrinsic types) is typing before semantics. We do not evaluate ill-typed terms. Programs written in these languages are mathematically guaranteed to be sound at all times.