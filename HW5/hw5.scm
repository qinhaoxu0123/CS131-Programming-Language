#lang racket 

(define (null-ld? obj)
  (if (pair? obj) 
      (if (eq? (car obj) (cdr obj)) #t #f)
      #f))
(define (ld? obj)
  (cond ((null-ld? obj) #t)
        ((not(pair? obj)) #f)
        ((null? (car obj)) #f)
        ((null? (cdr obj)) #t)
        ((not (pair? (car obj))) #f)
        (else (ld? (cons(cdr(car obj)) (cdr obj))))))

(define (cons-ld obj listdiff)
  (cons (cons obj (car listdiff)) (cdr listdiff)))

(define (car-ld listdiff)
  (cond((null-ld? listdiff) "error")
        ((ld? listdiff) (car(car listdiff)))
        (else "error")))

(define (cdr-ld listdiff)
  (cond
    ((null-ld? listdiff) "error")
    ((ld? listdiff) (cons (cdr (car listdiff)) (cdr listdiff)))
    (else "error")))

(define (ld . arg)
  (if (empty? arg) '(())
  (let ((newobj (list (car arg) )))
    (let ((newlst (append (cons (car arg) (cdr arg) ) newobj)))
      (cons newlst newobj)))))

(define (length-ld listdiff)
  (if (not (ld? listdiff)) "error"
      (let ((tl (cdr listdiff)))
        (let len ((ls (car listdiff)))
          (cond ((eq? ls tl) 0)
            (else (+ 1 (len (cdr ls)))))))))

(define (append-ld listdiff . args)
  (if (null? args) listdiff   
      (let objs-container ((ls (cons listdiff args)))
        (cond ((null? (cdr ls)) (car ls))
          (else (let cons-obj ((lsdiff (ld->list (car ls))))
                  (if (null? lsdiff)
                      (objs-container (cdr ls))
                      (cons-ld (car lsdiff) (cons-obj (cdr lsdiff))))))))))

(define (list->ld list)
	(cond ((not (list? list)) "error")
		((null? list) (cons list list))
                (else (cons list null))))

(define (ld->list listdiff)
  (cond ((null-ld? listdiff) null)
    ((not (ld? listdiff)) "error")
    (else (let ((tl (cdr listdiff)))
            (let gn-ls ( (ls (car listdiff) ) )
              (if (eq? tl ls) null
                  (cons (car ls) (gn-ls (cdr ls)))))))))


(define (ld-tail listdiff k)
   (cond ((> k (length-ld listdiff)) "error")
    ((< k 0) "error")
    (else(if (equal? k 0) 
                listdiff
	     (ld-tail (cdr-ld listdiff) (- k 1))))))


(define (my-map proc lst)
  (if (null? lst) '()
        (cons (proc (car lst)) (my-map proc (cdr lst)))))
        
(define (map-ld proc . lst)
  (if (null? (car lst)) '()
             (let ((listHolder
                    (let mapTo ( (procHolder proc) (lstHolder lst) (len (length-ld (car lst))) )
                      (if (= len 0) '()
                          (cons (apply procHolder (my-map car-ld lstHolder))
                                (mapTo procHolder (my-map cdr-ld lstHolder) (- len 1)))))))
   (list->ld listHolder))))
      
 (define (expr2ld expr)
  (cond ((null? expr) '())
        ((pair? (car expr)) (cons(expr2ld (car expr)) (expr2ld (cdr expr))))
        ( (equal? 'list? (car expr)) (cons 'ld? (expr2ld(cdr expr))))
        ( (equal? 'cdr (car expr)) (cons 'cdr-ld (expr2ld(cdr expr))))
        ( (equal? 'list (car expr)) (cons 'ld (expr2ld(cdr expr))))
        ( (equal? 'car (car expr)) (cons 'car-ld (expr2ld(cdr expr))))
        ( (equal? 'cons (car expr)) (cons 'cons-ld (expr2ld(cdr expr))))
        ( (equal? 'length (car expr)) (cons 'length-ld (expr2ld(cdr expr))))
        ( (equal? 'append (car expr)) (cons 'append-ld (expr2ld(cdr expr))))
        ( (equal? 'list-tail (car expr)) (cons 'ld-tail (expr2ld(cdr expr))))
        ( (equal? 'map (car expr)) (cons 'map-ld (expr2ld(cdr expr))))
        ( else (cons(car expr) (expr2ld (cdr expr))))))



