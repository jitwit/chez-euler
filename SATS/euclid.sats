#include "share/atspre_staload.hats"
staload "prelude/SATS/arith_prf.sats"
staload "prelude/SATS/integer.sats"

fn rem_gez {n,m:pos} (n:int(n),m:int(m)): [r:nat | m>r] int(r)
fn euclid {m,n:pos} (m:int(m),n:int(n)) : [r:pos] int(r)
prfun algorithm_E {m,n:pos} (m:int(m),n:int(n)) 
: [a,b,r:int | a*m + b*n == r; r > 0] (int(a),int(b),int(r))

