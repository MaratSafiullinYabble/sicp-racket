#lang sicp

(#%require rackunit)

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "bad tagged data -- TYPE-TAG" datum)))

(define Ben
  (list (cons 'name "Bitdiddle Ben")
        (cons 'address "Slumerville")
        (cons 'position "computer wizard")
        (cons 'salary "60000")))
(define Alyssa
  (list (cons 'name "Hacker Alyssa P")
        (cons 'address "Cambridge")
        (cons 'position "computer programmer")
        (cons 'salary "40000")))

(define ins-div1-records (list Ben Alyssa))

(define Fect (list "Fect Cy D" (list "computer programmer" "Cambridge" 35000)))
(define Tweakit (list "Tweakit Lem E" (list "computer technician" "Boston" 25000)))

(define ins-div2-records (list Fect Tweakit))

(define typed-ins1 (attach-tag 'ins1-record ins-div1-records))

(define typed-ins2 (attach-tag 'ins2-record ins-div2-records))

;--------------------------------------------------------------

(define (find-employee-record-ins-1 emp-list emp-name)
  (cond
    [(null? emp-list) '()]
    [(eq? (field-val-ins-1 (car emp-list) 'name) emp-name) (car emp-list)]
    [else (find-employee-record-ins-1 (cdr emp-list) emp-name)]))

(define (get-salary-ins-1 record)
  (field-val-ins-1 record 'salary))

(define (field-val-ins-1 record field-name)
  (cond
    [(null? record) '()]
    [(eq? (caar record) field-name) (cdar record)]
    [else (field-val-ins-1 (cdr record) field-name)]))

(define (find-employee-record-ins-2 emp-list emp-name)
  (define (name-eq? record)
    (if (eq? (car record) emp-name) #t #f))
  (cond
    [(null? emp-list) '()]
    [(name-eq? (car emp-list)) (car emp-list)]
    [else (find-employee-record-ins-2 (cdr emp-list) emp-name)]))

(define (get-salary-ins-2 record)
  (list-ref (list-ref record 1) 2))

(define (find-employee-record list-of-files emp-name)
  (let ([record (get-record (car list-of-files) emp-name)])
    (cond
      [(not (null? record)) (cdr record)]
      [(null? list-of-files) '()]
      [else (find-employee-record (cdr list-of-files) emp-name)])))

(define (get-record file emp-name)
  (define (find-in-file file)
    (cond
      [(eq? (type-tag file) 'ins1-record) (find-employee-record-ins-1 (cdr file) emp-name)]
      [(eq? (type-tag file) 'ins2-record) (find-employee-record-ins-2 (cdr file) emp-name)]
      [else (error "unknown tag -- GET-RECORD" file)]))
  (let ([record (find-in-file file)])
    (if (null? record)
        '()
        (attach-tag (type-tag file) record))))

(define (get-salary record)
  (cond
    [(eq? (type-tag record) 'ins1-record) (get-salary-ins-1 (cdr record))]
    [(eq? (type-tag record) 'ins2-record) (get-salary-ins-2 (cdr record))]
    [else (error "unknown tag -- GET-SALARY" record)]))

;--------------------------------------------------------------

(check-equal? (find-employee-record (list typed-ins1 typed-ins2) "Bitdiddle Ben") Ben)
(check-equal? (find-employee-record (list typed-ins1 typed-ins2) "Tweakit Lem E") Tweakit)
(check-equal? (cdr (get-record typed-ins1 "Hacker Alyssa P")) Alyssa)
(check-equal? (get-record typed-ins2 "Hacker Alyssa P") '())
(check-equal? (cdr (get-record typed-ins2 "Fect Cy D")) Fect)
(check-equal? (get-record typed-ins1 "Fect Cy D") '())
(check-equal? (get-salary (get-record typed-ins1 "Bitdiddle Ben")) "60000")
(check-equal? (get-salary (get-record typed-ins2 "Fect Cy D")) 35000)
