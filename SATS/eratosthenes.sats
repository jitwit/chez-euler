

prfun sqrt_lte {j:nat} () :<prf> [j <= j*j] void
prfun sqrt_bound {j,N:nat | 0 < N; j*j < N} () :<prf> [ j < N ] void



