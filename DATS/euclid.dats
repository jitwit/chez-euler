staload "./../SATS/euclid.sats"
staload "prelude/SATS/arith_prf.sats"
staload "prelude/SATS/integer.sats"
staload UN = "prelude/SATS/unsafe.sats"

(* {n,m:pos} (n:int(n),m:int(m)): [r:nat | m>r] int(r) =  *)
implement rem_gez (n,m) = $UN.cast(n%m)

primplmnt div_tra{x,y,z,a,b} (pfxy, pfyz) =
let prval p1 = div_elim pfxy
    prval p2 = div_elim pfyz
in p2 end


(* {m,n:pos} (m:int(m),n:int(n)) : [r:pos] int(r) *)
implement euclid (m,n) =
let fun lp {m,n:nat | m > 0}.<n>. (m:int(m),n:int(n)) : [r:pos] int(r) = 
  if n > 0 then lp (n,rem_gez(m,n)) else m
in lp(m,n) end

fn eeuclid(m,n:int) : (int,int,int) =
let fun lp (m,n,a,_a,b,_b:int) : (int,int,int) = 
  case n of | 0 => (m,b,a) | _ => let val q:int = m \g0int_div n in
    lp(n,m \g0int_sub (q \g0int_mul n),_a,a\g0int_sub (q \g0int_mul _a)
     ,_b,b\g0int_sub(q\g0int_mul _b)) end
 in lp (m,n,0,1,1,0) end

