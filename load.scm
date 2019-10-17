(import (prefix (patricia-set) s:)
	(patricia))

(define source-files
  '("outils.scm"
    "digits.scm"
    "eratosthenes.scm"
    "euclid.scm"
    "miller-rabin.scm"
    "pollard.scm"
    "lazy.scm"
    "sequences.scm"
    "polynomial.scm"
    "geometry.scm"
    "pseudo.scm"
    "continued-fraction.scm"
    "combinatorics.scm"))

(for-each load source-files)
