(define pi:get
  (lambda (P)
    (cdr P)))

(define pi:get-inverse
  (lambda (P)
    (car P)))

(define pi:permutation
  (lambda (P P^-1)
    (cons P^-1 P)))

(define pi:invert
  (lambda (P)
    (pi:permutation (pi:get-inverse P)
		    (pi:get P)
		    )))

(define pi:id
  (cons empty-tree empty-tree))

(define pi:pair
  (lambda (i j)
    (let ((P (merge-with (lambda (x y) x)
			 (singleton i j)
			 (singleton j i))))
      (pi:permutation P P))))

(define pi:image
  (lambda (j P)
    (lookup-with-default j j (pi:get P))))

(define pi:source
  (lambda (j P)
    (lookup-with-default j j (pi:get-inverse P))))

(define pi:tree-invert
  (lambda (T)
    (tree-ifold-right (lambda (i j P)
			(if (= i j)
			    P
			    (insert j i P)))
		      empty-tree
		      T)))

(define tree->permutation
  (lambda (T)
    (let ((T^-1 (pi:tree-invert T)))
      (pi:permutation T^-1 (pi:tree-invert T^-1)))))

(define eg
  (let ((n 16)
	(g 3))
    (fold-right (lambda (x T)
		  (insert x (mod (* x g) n) T))
		empty-tree
		(Z/nZ* n))))
