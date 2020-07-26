#include "share/atspre_staload.hats"
staload "prelude/SATS/arith_prf.sats"
staload "prelude/SATS/integer.sats"

(* 
praxi
divmod_elim
  {x,y:int | x >= 0; y > 0}
  {q,r:int}
(
  pf: DIVMOD (x, y, q, r)
) : [qy:nat | 0 <= r; r < y; x==qy+r] MUL (q, y, qy)

praxi
divmod_mul_elim
  {x,y:int | x >= 0; y > 0}
  {q,r:int}
  (pf: DIVMOD (x, y, q, r))
: [0 <= q; 0 <= r; r < y; q==ndiv_int_int(x, y); x==q*y+r] void
// end of [divmod_mul_elim]
*)

absprop DIVIDES (int,int)
// = {d,x:int | x >= 0; d > 0}
//  DIVIDES (d,x) of [q:int | d*q == x] ()

praxi divides_elim {x:int}{d:pos} (pf:DIVIDES(d,x)) : [k:int] MUL(k,d,x)
praxi divides_intro{k,d,x:int} (pf : MUL(k,d,x)) () : DIVIDES(d,x)

prfun div_trans {a,b,c:int}{p,q:int}{pq:int} (pfab : MUL(a,p,b), pfbc : MUL(b,q,c))
 :<prf> MUL (a,pq,c)

dataprop GCD (int,int,int)
= {m,n,d:nat}
  GCD (m,n,d)
  of ()



dataprop EUC (int,int)
= {m:pos} EUCB (m,0) of ()
| {m,r:pos}{n:nat} EUCI (n,r) of [q:nat] (DIVMOD(m,n,q,r),EQINT(r,m-n*q))

// g1int_nmod2
fn rem_gez {n,m:pos} (n:int(n),m:int(m)): [r:nat | m>r] int(r)
fn euclid {m,n:pos} (m:int(m),n:int(n)) : [r:pos] int(r)
fn algorithm_E {m,n:pos} (m:int(m),n:int(n)) 
: [a,b,r:int] (int(a),int(b),int(r))

// sortdef elt = int
// abst@ype ELT (a:t@ype, x:elt) = a
// absprop EDT (x: elt, y: elt, xy: elt) // abstract euclid division relation
// 
// dataprop
// EDTP (int, int, int) =
//   | {x:int} EDTB (x, 0, x)
//   | {x,y,r:int} EDTI (x,y+r,y) of EDT(x,y,r)
