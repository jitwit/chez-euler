#include "share/atspre_staload.hats"
staload "./../SATS/eratosthenes.sats"
staload "prelude/basics_pre.sats"
staload "prelude/SATS/arith_prf.sats"
staload "prelude/SATS/integer.sats"
staload "libats/SATS/bitvec.sats"
staload "libats/DATS/bitvec.dats"
staload UN = "prelude/SATS/unsafe.sats"

primplmnt sqrt_lte{j} () = sif j > 0
then mul_gte_gte_gte{j,j-1} ((* 1*j <= j*j *))
else let prval EQINT () = eqint_make{j,0} () in (* 0<=0*0 *) end
primplmnt sqrt_bound {j,N} () = sqrt_lte{j} ()

fun{} clear_from{i,N:nat | N > 3} (i: !int >> _,N:int(N), B : &bitvecptr(N) >> _): void = 
()

fun{} eratosthenes{N:nat | 9 < N} (N:int(N)): bitvecptr(N) = 
let
    var B = bitvecptr_make_full(N)
    var i:int
    var j:int
    var b_i:int
    fun outer{j,N:nat | j*j < N; 9 < N}
        (j:int(j), N:int(N), B: &bitvecptr(N) >> _) : void =
        let val k = g1ofg0(j*j)
            val _j = j+2
            prval prf = sqrt_bound{j,N} ()
            val _ = if 1 = B[j] then if k < N then () // inner(k,j+j,N,B)
        in if _j * _j < N then if _j < N then outer(_j,N,B) end
in B[0] := 0; B[1] := 0;
   outer(3,N,B);
//   for (i := 3, b_i := B[i];i*i < N;i := i + 2) begin
//   if 1 = [i] then $extfcall(void,"printf","%3d : %d\n",i,b_i)
   // print(i); ((" ":string)); print(j)
//     j := bitvecptr_get_at(bits,i);
//   end; B
   B
end

// val _ = bitvecptr_free(B)
// val _ = println!(B[0],B[1],B[2])

implement main0 () = {
  var B = eratosthenes (30)
  var j:int
  var b_j:int
  val _ = for (j := 0, b_j := B[j]; j < 30; j := j + 1)
           begin $extfcall(void,"printf","%3d : %d\n",j,b_j) end
  val _ = bitvecptr_free(B)

}

