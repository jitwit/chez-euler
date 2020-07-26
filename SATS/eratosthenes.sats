

prfun sqrt_lte {j:nat} () :<prf> [j <= j*j] void
prfun sqrt_bound {j,N:nat | 0 < N; j*j < N} () :<prf> [ j < N ] void
// apparently can't figure out 0 <= j*j...
prfun sqr_gez {j:int} () :<prf> [0 <= j*j] void



