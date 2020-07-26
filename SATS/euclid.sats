#include "share/atspre_staload.hats"
staload "prelude/SATS/arith_prf.sats"
staload "prelude/SATS/integer.sats"

fn rem_gez {n,m:pos} (n:int(n),m:int(m)): [r:nat | m>r] int(r)
fn euclid {m,n:pos} (m:int(m),n:int(n)) : [r:pos] int(r)
fn algorithm_E {m,n:pos} (m:int(m),n:int(n)) 
: [a,b,r:int] (int(a),int(b),int(r))

praxi div_elim{a,b,c:int} (pf : DIV(a,b,c)) : DIVMOD(a,b,c,0)

prfun div_tra{x,y,z,a,b:int} (pfxy : DIV (y,x,a), pfyz : DIV(z,y,b))
:<prf> DIV(z,x,a*b)

