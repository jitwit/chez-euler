
(define delete-single
  (lambda (item lst)
    (cond ((null? lst) lst)
	  ((eq? item (car lst))
	   (cdr lst))
	  (else
	   (cons (car lst) (delete-single item (cdr lst)))))))

(define maximum-on
  (lambda (X h)
    (let ((best (car X))
          (h-best (h (car X))))
      (for-all (lambda (x)
                 (let ((h-x (h x)))
                   (when (< h-best h-x)
                     (set! h-best h-x)
                     (set! best x))))
               (cdr X))
      (cons h-best best))))

(define maximums-on
  (lambda (X h)
    (if (null? X)
        '()
        (let ((ms (list (car X)))
              (best (h (car X))))
          (let loop ((X (cdr X)))
            (if (null? X)
                ms
                (let ((curr (h (car X))))
                  (cond ((= curr best)
                         (push! (car X) ms))
                        ((> curr best)
                         (set! best curr)
                         (set! ms (list (car X)))))
                  (loop (cdr X)))))))))

(define sort-on
  (lambda (heuristic X)
    (sort (lambda (x y)
	    (< (heuristic x)
	       (heuristic y)))
	  X)))

(define rank-on
  (lambda (heuristic X)
    (sort-on car (map (lambda (x)
                        (cons (heuristic x) x))
                      X))))

(define sort-on!
  (lambda (heuristic X)
    (sort! (lambda (x y)
	     (< (heuristic x)
		(heuristic y)))
	   X)))

(define subsets
  (lambda (X)
    (if (null? X)
        '(())
        (let ((P-X (subsets (cdr X))))
          (append (map (lambda (U)
                         (cons (car X) U))
                       P-X)
                  P-X)))))

(define count
  (lambda (predicate X)
    (fold-left (lambda (cnt x)
                 (if (predicate x)
                     (fx1+ cnt)
                     cnt))
               0
               X)))

(define filter-map
  (lambda (predicate X)
    (fold-right (lambda (x xs)
                  (let ((p-x (predicate x)))
                    (if p-x
                        (cons p-x xs)
                        xs)))
                '()
                X)))

(define shuffle
  (lambda (X)
    (let ((V (list->vector X)))
      (shuffle! V)
      (vector->list V))))

(define nub-eq
  (lambda (X)
    (let ((table (make-hash-table)))
      (for-each (lambda (x)
                  (hashtable-set! table x #t))
                X)
      (vector->list
       (hashtable-keys table)))))

(define nub-equal
  (lambda (X)
    (let ((table (make-hashtable equal-hash equal?)))
      (for-each (lambda (x)
                  (hashtable-set! table x #t))
                X)
      (vector->list
       (hashtable-keys table)))))

(define eq-histogram
  (lambda (X)
    (let ((table (make-hash-table)))
      (for-each (lambda (x)
                  (hashtable-update! table
                                     x
                                     fx1+
                                     0))
                X)
      (vector->list
       (hashtable-cells table)))))

(define equal-histogram
  (lambda (X)
    (let ((table (make-hashtable equal-hash equal?)))
      (for-each (lambda (x)
                  (hashtable-update! table
                                     x
                                     fx1+
                                     0))
                X)
      (vector->list
       (hashtable-cells table)))))

(define suffixes
  (lambda (X)
    (if (null? X)
        '(())
        (cons X (suffixes (cdr X))))))

(define take-while
  (lambda (predicate X)
    (letrec ((aux (lambda (X)
                    (if (and (pair? X)
                             (predicate (car X)))
                        (cons (car X) (aux (cdr X)))
                        '()))))
      (aux X))))

(define drop-while
  (lambda (predicate X)
    (letrec ((aux (lambda (X)
                    (if (and (pair? X)
                             (predicate (car X)))
                        (aux (cdr X))
                        X))))
      (aux X))))

(define group-with
  (lambda (equivalence X)
    (letrec ((aux (lambda (X G g)
                    (if (pair? X)
                        (let ((x (car X)))
                          (if (equivalence g x)
                              (aux (cdr X)
                                   (cons x G)
                                   x)
                              (cons (reverse G) (grp X))))
                        (list (reverse G)))))
             (grp (lambda (X)
                    (if (pair? X)
                        (aux (cdr X)
                             (list (car X))
                             (car X))
                        '()))))
      (grp X))))

(define (intersperse sep xs)
  (if (or (null? xs)
          (null? (cdr xs)))
      xs
      (cdr
       (fold-right (lambda (x y)
                     (cons* sep x y))
                   '()
                   xs))))

