#lang sicp

(#%require rackunit)

(define (tree-map proc tree)
  (define (proc-tree sub-tree)
    (if (list? sub-tree)
        (tree-map proc sub-tree)
        (proc sub-tree)))
  (map proc-tree tree))

(define tree (tree-map (lambda (x) (* 2 x)) (list 1 2 3 (list 4 5 6 (list 7 8 9)))))

(check-equal? 4 (list-ref tree 1))
(check-equal? 8 (list-ref (list-ref tree 3) 0))
(check-equal? 18 (list-ref (list-ref (list-ref tree 3) 3) 2))
