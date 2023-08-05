## Sets, Relations, Functions

the set of **natural numbers** { 0, 1, 2, 3, ... } is denoted by &#8469;

**n-place relation** = R &subseteq; S1 x S2 x ... Sn

(s1, s2, ... sn) are *related* by R if (s1, s2, ... sn) &isin; R

**one-place relation** = predicate

P is true of s &isin; S, if s &isin; P. Also written as P(s)

* let P = is_even
* 0 &isin; is_even, 2 &isin; even
* P = { 0, 2, 4, ... }

**two-place relation** = binary relation.

s &isin; S; t &isin; T; s R t if (s, r) &isin; R

**domain** = the set being mapped from in a binary relation

**codomain** = the set being mapped to in a binary relation

s &isin; S; t &isin; T; (s, r) &isin; R

dom(R) = s &isin; S, such that (s, t) &isin; R

range(R) = t &isin; T, such that (s, t) &isin; R

**function** = each domain member is mapped to only one codomain member

(s, t1) &isin; R and (s, t2) &isin; R implies t1 = t2,

**partial function**

the book defines partial functions (not just *functions*!) as the scenario in which (s, t1) &isin; R and (s, t2) &isin; R implies t1 = t2,
but this is the definition of being well-defined - each input maps to a unique output.

**total function**

a function for which dom(R) = S

**injective** = each codomain element is only being mapped to once

s1, s2 &isin; S, t1, t2 &isin; T, s1 R t1, s2 R t2, s1 &ne; s2 implies t1 &ne; t2

**surjective** = the whole codomain is being mapped

range(R) = T

**divergence** = a function that does not terminate

*this should imply a lack of totality*

**failure** = a function that produces special outputs for certain inputs that fall outside its codomain

*since the failure-signaling outputs are not part of the codomain, it means that not all values of the domain are mapped to the codomain, meaning that failure also implies a lack of totality*

a predicate P is **preserved** by the binary relation R on S, if for s R s', s &isin; S, s' &isin; and P(s) implies P(s')

## Ordered Sets

**reflexivity** = R is reflexive if &forall; s &isin; S, (s, s) &isin; R

**symmetry** = R is symmetric if &forall; s, t &isin; S, s R t implies t R s

**antisymmetry** = R is antisymmetric if &forall; s, t &isin; S, s R t and t R s implies s = t

**transitivity** = R is transitive if &forall; s, t, u &isin; S, s R t and t R u implies s R u

**preorder** = *reflexive* and *transitive* relation - &lt; or &le;

**partial order** = *antisymmetric* preorder - &le;

**total order** = &forall; s, t &isin; S, s &le; t or t &le; s

**equivalence** = *reflexive*, *transitive* and *symmetric* relation

**reflexive closure** = the smallest reflexive relation R' that contains R

**transitive closure** = the smallest transitive relation R+ that contains R

**reflexive transitive closure** = the smallest reflexive transitive relation R* that contains R

### 2.2.6
**Given a relation R on S, and R' = R &cup; { (s, s) &isin; S }, show that R' is the reflexive closure of R.**

E = { (s, s) &isin; S } is by definition the minimal reflexive relation a set can have. The union of this set with any other relation on S is guaranteed to be reflexive. Since E is minimal, there is no smaller set R &cup; E can have that still maintains reflexivity. Thus R' is the reflexive closure of R. 

### 2.2.7
**Consider the following sequence of sets of pairs:**

**R_0 = R**

**R_i+1 = R_i &cup; { (s, u) &exist; t, (s, t) &isin; R_i and (t, u) &isin; R_i }**

**Show that R+ is the transitive closure for R, where R+ = R_0 &cup; R_1 &cup; ... &cup; R_i**

Given that R_0 = R, we can say that R &subseteq; R+. R may or may not initialy be reflexive. However, through the iterative process (Warshall's algorithm?) defined for R+, we ensure that at each step, the only elements being added to the set are built upon the transitive property, and all possible transitive (s, u) pairs are being added. The process stops once there no longer is any t for which (s, t) and (t, u) exists, which proves R+ is the transitive closure for R. 

### 2.2.8
**Suppose R is a binary relation on S and P is a predicate on S that is preserved by R.**

**Show that P is also preserved by R\*.**

R* = R &cup; R' &cup; R+

Since P is preserved by R, we need to show that P is also preserved by R' and R+

If s &isin; P, P is preserved by R'.

&forall; s, t &isin; S, such that (s, t) &isin; R, we have that s &isin; P and t &isin; P.

Since &forall; s, t, u &isin; R+, s, t, u &isin; S also, we can say that P is also preserved by R+.

## Sequences

**sequence** = list of elements, separated by commas

`cons` and `append` operations denoted by commas

sequence of numbers from 1 to n is denoted by 1..n

|a| is the length of sequence a

**permutation** = a variation of another sequence, possibly in a different order

## Induction

induction seems to be based on first-order logic:

if P(0)

and &forall; i we have P(i) -> P(i + 1)

then &forall; n we have P(n)

