(define-syntax test-checko
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (error 'test-check
               "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
               'tested-expression expected produced)))))))

(define (rembero e l r)
  (conde
   ((== l '()) (== r '()))
   ((fresh (x xs ys)
	   (== l `(,x . ,xs))
	   (== e x)
	   (== ys r)
	   (rembero e xs ys)))
   ((fresh (x xs ys)
	   (== l `(,x . ,xs))
	   (=/= e x)
	   (== `(,x . ,ys) r)
	   (rembero e xs ys)))))


(test-checko 'xy (run 1 (q) (== q 'x) (=/= q 'y))
	     '((x (and (or)))))

(test-checko 'xx (run 1 (q) (== q 'x) (=/= q 'x))
	     '())

(test-checko 'yy (run 1 (q) (=/= q 'y) (== q 'y))
	     '())

(test-checko 'yx (run 1 (q) (=/= q 'y) (== q 'x))
	     '((x (and (or)))))

(test-checko 'u-v-different
	     (run* (q) (fresh (u v) (=/= q `(,u ,v)) (== q `(,v ,u)) (membero u '(x y)) (membero v '(x y))))
	     '(((y x) (and (or))) ((x y) (and (or)))))

