(import (prefix (patricia-set) s:)
	(patricia))

(define source-files
  '("code/outils.scm"
    "code/digits.scm"
    "code/eratosthenes.scm"
    "code/euclid.scm"
    "code/miller-rabin.scm"
    "code/pollard.scm"
    "code/jacobi.scm"
    "code/lazy.scm"
    "code/sequences.scm"
    "code/polynomial.scm"
    "code/geometry.scm"
    "code/pseudo.scm"
    "code/continued-fraction.scm"
    "code/combinatorics.scm"
    "code/lists.scm"
    "code/statistics.scm"))

(for-each load source-files)
