#include "share/atspre_staload.hats"
staload "./../SATS/eratosthenes.sats"
staload "libats/SATS/bitvec.sats"
staload "libats/DATS/bitvec.dats"

primplmnt sqrt_lte{j} () = sif j > 0
then mul_gte_gte_gte{j,j-1} ((* 1*j <= j*j *))
else let prval EQINT () = eqint_make{j,0} () in (* 0<=0*0 *) end

primplmnt sqrt_bound {j,N} () = sqrt_lte{j} ()
primplmnt sqr_gez {j} () = 
sif j < 0 then mul_lte_lte_gte{j,j} ()
else mul_gte_gte_gte{j,j} ((* duh *))

fun{} eratosthenes{N:nat|2 <= N} (N:int(N)) : [pi_N:nat] list_vt(int,pi_N) =
let val B = bitvecptr_make_full(1+(N-3/2))
    fun inner{j,N,dj:nat | 0 < dj; j < N; 3 <= j}.<N-j>.
        (j:int(j),dj:int(dj), N:int(N), B: !bitvecptr(N) >> _) : void =
        let val _ = B[(j-3)/2] := 0
        in if j+dj < N then inner(j+dj,dj,N,B) end
    fun outer{j,N:nat | 3 <= j; j*j < N; 9 < N }.<N-j*j>.
        (j:int(j), N:int(N), B: !bitvecptr(N) >> _) : void =
        let prval _ = sqrt_lte{j} ((* help solve j*j < N => j < N *))
            val _ = if int2bool0(B[(j-3)/2]) then inner(j*j,j+j,N,B)
        in if (j+2)*(j+2) < N then outer(j+2,N,B) end
    fun final{j,N,k:nat | j < N} .<j>.
        (j:int(j), B : !bitvecptr(N) >> _, primes : list_vt(int,k))
        : listGte_vt(int,k) =
        if j-1 >= 2
        then if int2bool0(B[(j-3)/2])
             then final(j-2,B,list_vt_cons(j,primes))
             else final(j-2,B,primes)
        else list_vt_cons(2,primes)
    val _ = if 9<N then outer (3,N,B)
    val primes = final(N-1,B,list_vt_nil())
    val _ = bitvecptr_free(B)
in primes end

implement main0 () = {
  var primes = eratosthenes (2000000)
  val _ = println!(length(primes))
  val _ = list_vt_free(primes)
//  val _ = primes := list_vt_reverse(primes)
//  val _ = case primes of
//  | ~list_vt_cons(p,ps) => (println!(p); list_vt_free(ps))
//  | ~list_vt_nil() => println!("oops")
}
