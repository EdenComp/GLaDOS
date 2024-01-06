(define y 12)

(define add (lambda (a b) (+ a b)))

(define multiply-by-three (lambda (c) (* c 3)))

(define multiply-by-two (lambda (c) (* c 2)))

(define squared-result
  (add (multiply-by-two (multiply-by-three y)) 3))

(define final-result
  (- squared-result 9))

(+ (- (/ final-result 3) 1) (- (/ final-result 3) 1))
