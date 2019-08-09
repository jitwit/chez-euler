(define point
  (lambda (x y)
    (vector x y)))

(define segment
  (lambda (x1 y1 x2 y2)
    (vector (point x1 y1) (point x2 y2))))

(define segment-between
  (lambda (z1 z2)
    (vector z1 z2)))

(define _x
  (lambda (V)
    (vector-ref V 0)))

(define _y
  (lambda (V)
    (vector-ref V 1)))

(define _start
  (lambda (V)
    (vector-ref V 0)))

(define _end
  (lambda (V)
    (vector-ref V 1)))

(define segment-values
  (lambda (segment)
    (let* ((p (_start segment))
	   (q (_end segment))
	   (x1 (_x p))
	   (y1 (_y p))
	   (x2 (_x q))
	   (y2 (_y q))
	   (A (- y2 y1))
	   (B (- x1 x2))
	   (C (+ (* A x1) (* B y1))))
      (values x1 y1 x2 y2 A B C))))

(define open-in-bounds
  (lambda (X Y x1 y1 x2 y2)
    (or (and (< (min x1 x2) X)
	     (< X (max x1 x2)))
	(and (< (min y1 y2) Y)
	     (< Y (max y1 y2) )))))

(define closed-in-bounds
  (lambda (X Y x1 y1 x2 y2)
    (or (and (<= (min x1 x2) X)
	     (<= X (max x1 x2)))
	(and (<= (min y1 y2) Y)
	     (<= Y (max y1 y2) )))))

(define open-segment-intersection
  (lambda (s1 s2)
    (define-values (x11 y11 x12 y12 A1 B1 C1) (segment-values s1))
    (define-values (x21 y21 x22 y22 A2 B2 C2) (segment-values s2))
    (define det (- (* A1 B2) (* A2 B1)))
    (if (= 0 det)
	#f
	(let ((X (/ (- (* B2 C1) (* B1 C2)) det))
	      (Y (/ (- (* A1 C2) (* A2 C1)) det)))
	  (if (and (open-in-bounds X Y x11 y11 x12 y12)
		   (open-in-bounds X Y x21 y21 x22 y22))
	      (point X Y)
	      #f)))))

(define closed-segment-intersection
  (lambda (s1 s2)
    (define-values (x11 y11 x12 y12 A1 B1 C1) (segment-values s1))
    (define-values (x21 y21 x22 y22 A2 B2 C2) (segment-values s2))
    (let ((det (- (* A1 B2) (* A2 B1))))
      (if (= 0 det)
	  #f
	  (let ((X (/ (- (* B2 C1) (* B1 C2)) det))
		(Y (/ (- (* A1 C2) (* A2 C1)) det)))
	    (if (and (closed-in-bounds X Y x11 y11 x12 y12)
		     (closed-in-bounds X Y x21 y21 x22 y22))
		(point X Y)
		#f))))))

(define S1 (segment 27 44 12 32))
(define S2 (segment 46 53 17 62))
(define S3 (segment 46 70 22 40))
