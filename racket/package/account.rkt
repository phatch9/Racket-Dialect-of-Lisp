#lang racket

(require racket/contract)

(struct account (balance))

(provide
  (contract-out
    [new-account account?]
    [balance (-> account? number?)]
    [withdraw (-> account? (and/c number? positive?) account?)]
    [deposit (-> account? (and/c number? positive?) account?)]))

;; A new, empty account
(define new-account (account 0))

;; Get the current balance
(define (balance acc)
  (if (account? acc)
      (account-balance acc)
      (error "Invalid account")))

;; Withdraw funds from an account
;; error handling
(define (withdraw acc amt)
  (cond
    [(not (account? acc)) (error "Invalid account")] 
    [(not (positive? amt)) (error "Amount must be positive")]
    [(> amt (account-balance acc)) (error "Insufficient funds")]
    [else (account (- (account-balance acc) amt))]))

;; Add funds to an account
;; Verify only positive numbers are passed as the second argument to withdraw and deposit.
(define (deposit acc amt)
  (cond
    [(not (account? acc)) (error "Invalid account")]
    [(not (positive? amt)) (error "Deposit must be positive")]
    [else (account (+ (account-balance acc) amt))]))

;; test case 

(require "account.rkt")

(define my-account new-account)

;; Good examples
(displayln (balance (deposit my-account 40)))
(displayln (balance (withdraw (deposit my-account 40) 25)))

;; Bad examples
(displayln (balance (deposit my-account -40)))
(displayln (balance (withdraw (deposit my-account 40) 95)))
(displayln (balance (deposit my-account "42")))
