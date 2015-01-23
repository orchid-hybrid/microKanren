;; Jason Hemann and Dan Friedman
;; microKanren, final implementation from paper

;; tools

(define (all p l)
  (if (null? l)
      #t
      (if (p (car l))
	  (all p (cdr l))
	  #f)))

(define (any p l)
  (if (null? l)
      #f
      (if (p (car l))
	  #t
	  (any p (cdr l)))))

;; monoid

(define mzero '())
(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $2 ($1))))
    (else (cons (car $1) (mplus (cdr $1) $2)))))


;; monad

(define (unit s/c) (cons s/c mzero))
(define (bind $ g)
  (cond
    ((null? $) mzero)
    ((procedure? $) (lambda () (bind ($) g)))
    (else (mplus (g (car $)) (bind (cdr $) g)))))

(define (mapm f l)
  (if (null? l)
      (unit '())
      (bind (f (car l))
	    (lambda (v)
	      (bind (mapm f (cdr l))
		    (lambda (vs)
		      (unit (cons v vs))))))))

;; unification

(define (var c) (vector c))
(define (var? x) (vector? x))
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

(define (walk u s)
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

(define (ext-s x v s) `((,x . ,v) . ,s))

(define (== u v)
  (lambda (s/c/d)
    (let ((s (unify u v (car s/c/d))))
      (if s
	  (let ((c (cadr s/c/d))
		(d (caddr s/c/d)))
	    (normalize-disequality-store s c d))
	  mzero))))

(define (normalize-disequality-store s c d)
  ;; the disequality store d is of the form
  ;;      (AND (OR (=/= ...) ...)
  ;;           (OR (=/= ...) ...) ...)
  ;; by de-morgan this can be interpreted as
  ;; (NOT (OR (AND (== ...) ...)
  ;;          (AND (== ...) ...) ...))
  ;; so to normalize we can normalize each
  ;; part of the OR individually (failing if
  ;; any one of them fails), but we need to
  ;; chain each unification in the AND's alt-
  ;; ernatively (and this is what we do here)
  ;; merge them into a single unification op
  (bind (mapm (lambda (es)
		(let ((d^ (disequality (map car es) (map cdr es) s)))
		  (if d^
		      (unit d^)
		      mzero)))
	      d)
	(lambda (d)
	  (unit `(,s ,c ,d)))))

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else (and (eqv? u v) s)))))

(define (rem-s m b)
  (if (null? m)
      '()
      (if (equal? (car m) b)
	  (cdr m)
	  (cons (car m) (rem-s (cdr m) b)))))

(define (sub-s m n)
  ;; m - n
  (if (null? n)
      m
      (sub-s (rem-s m (car n)) (cdr n))))

(define (disequality u v s)
  (let ((s^ (unify u v s)))
    (if s^
	(let ((s^^ (sub-s s^ s))) ;; TODO make unification return a difflist so we don't have to subtract
	  (if (null? s^^)
	      #f
	      s^^))
	'())))

(define (=/= u v)
  (lambda (s/c/d)
    (let* ((s (car s/c/d))
	   (c (cadr s/c/d))
	   (d (caddr s/c/d))
	   (d^ (disequality u v s)))
      (if d^
	  (unit `(,s ,c ,(cons d^ d)))
	  mzero))))

;; language constructs

(define (call/fresh f)
  (lambda (s/c/d)
    (let ((c (cadr s/c/d)))
      ((f (var c)) `(,(car s/c/d) ,(+ c 1) ,(caddr s/c/d))))))

(define (disj g1 g2) (lambda (s/c/d) (mplus (g1 s/c/d) (g2 s/c/d))))
(define (conj g1 g2) (lambda (s/c/d) (bind (g1 s/c/d) g2)))
