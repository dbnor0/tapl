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

**partial function**

the book defines partial functions (not just *functions*!) as the scenario in which (s, t1) &isin; R and (s, t2) &isin; R implies t1 = t2,
but this is the definition of being well-defined - each input maps to a unique output.

**total function**

a function for which dom(R) = S


**divergence** = a function that does not terminate

*this should imply a lack of totality*

**failure** = a function that produces special outputs for certain inputs that fall outside its codomain

*since the failure-signaling outputs are not part of the codomain, it means that not all values of the domain are mapped to the codomain, meaning that failure also implies a lack of totality*

a predicate P is **preserved** by the binary relation R on S, if for s R s', s &isin; S, s' &isin; and P(s) implies P(s')

## Ordered Sets

