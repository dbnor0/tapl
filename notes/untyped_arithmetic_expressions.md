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