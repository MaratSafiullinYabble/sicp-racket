#lang racket

(require rackunit)
(require racket/exn)

;-------------------------------------------------------

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define failed-attempts-cnt 0)

  (define (call-the-cops)
    cops-message)

  (define (dispatch pass m)
    (if (eq? pass password)
        (begin
          (set! failed-attempts-cnt 0)
          (cond
            [(eq? m 'withdraw) withdraw]
            [(eq? m 'deposit) deposit]
            [else (error "Unknown call -- MAKE-ACCOUNT" m)]))
        (error (begin
                 (set! failed-attempts-cnt (+ failed-attempts-cnt 1))
                 (if (> failed-attempts-cnt 7) cops-message wrong-message)))))

  dispatch)

;-------------------------------------------------------

(define acc (make-account 100 'secret-password))

(define wrong-message "Wrong password")

(define cops-message "Cops called")

(define (attempt)
  (with-handlers ([exn:fail? (lambda (e) (exn->string e))])
    ((acc 'some-other-password 'deposit) 50)))

(check-equal? ((acc 'secret-password 'withdraw) 40) 60)
(check-equal? ((acc 'secret-password 'deposit) 40) 100)
; substring used because checking system has extra output
(check-equal? (substring (attempt) 0 14) wrong-message)
(check-equal? (substring (attempt) 0 14) wrong-message)
(check-equal? (substring (attempt) 0 14) wrong-message)
(check-equal? (substring (attempt) 0 14) wrong-message)
(check-equal? (substring (attempt) 0 14) wrong-message)
(check-equal? (substring (attempt) 0 14) wrong-message)
(check-equal? (substring (attempt) 0 14) wrong-message)
(check-equal? (substring (attempt) 0 11) cops-message)
