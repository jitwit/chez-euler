(import (prefix (chez patricia-set) s:))

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
    "control.scm"))

(for-each load source-files)
