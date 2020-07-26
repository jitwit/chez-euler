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
primplmnt sqr_gtz {j} () = mul_gte_gte_gte{j,j} ((* duh *))

fun{} eratosthenes{N:nat | 9 < N } (N:int(N)) : void = 
let var B = bitvecptr_make_full(N)
    var j:int
    var b_i:int
    fun inner{j,N,dj:nat | 0 < dj && j < N}.<N-j>.
        (j:int(j),dj:int(dj), N:int(N), B: !bitvecptr(N) >> _) : void =
        let val _ = B[j] := 0
        in if j+dj < N then inner(j+dj,dj,N,B) end
    fun outer{j,N:nat | 0 < j-2; j*j < N; 9 < N }.<N-j*j>.
        (j:int(j), N:int(N), B: !bitvecptr(N) >> _) : void =
        let prval prf = sqrt_bound{j,N} ()
            prval duh = sqr_gtz{j}()
            val _ = inner(j*j,j+j,N,B)
        in if (j+2)*(j+2) < N then outer(j+2,N,B) end
    fun final{j,N:nat | j < N} .<N-j>.
        (j:int(j), N:int(N), B : !bitvecptr(N) >> _) : void =
        let val _ = $extfcall(void,"printf","in %d %d\n",j,j%2 * B[j])
        in if 1+j < N then final(1+j,N,B) end
//    val _ = outer (3,N,B)
in outer(3,N,B); final(0,N,B);

   bitvecptr_free(B)
end
//   for (i := 3, b_i := B[i];i*i < N;i := i + 2) begin
//   if 1 = [i] then $extfcall(void,"printf","%3d : %d\n",i,b_i)
   // print(i); ((" ":string)); print(j)
//     j := bitvecptr_get_at(bits,i);
//   end; B

implement main0 () = {
  var _ = eratosthenes (90)
}

