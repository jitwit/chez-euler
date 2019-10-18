
;; knuth 4.2.2 pg 232.
(define (mean.stdev sample)
  (let ((n (vector-length sample)))
    (let loop ((m-k (+ 0. (vector-ref sample 0)))
               (s-k 0.)
               (k 1))
      (if (= k n)
          (cons m-k (sqrt (/ s-k n)))
          (let ((m-k+1 (+ m-k
                          (/ (- (vector-ref sample k) m-k)
                             (1+ k)))))
            (loop m-k+1
                  (+ s-k
                     (* (- (vector-ref sample k) m-k)
                        (- (vector-ref sample k) m-k+1)))
                  (1+ k)))))))
