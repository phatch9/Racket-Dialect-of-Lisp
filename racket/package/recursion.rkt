
;; Part 1 Implement a maxNum function.  This function reads in a list of numbers and returns the largest.
;; Define this function in a recursive manner.

;; Test solution at http://codecheck.it/files/18060406569ovaum1rpro2z1io6yccwcjanLinks to an external site


;; Part 2. Implement the "fizzbuzz" game.  The function counts from 1 to the specified number, returning a
string with the result.  The rules are:

    If a number is divisible by 3 and by 5, instead say "fizzbuzz"
    Else if a number is divisible by 3, instead say "fizz"
    Else if a number is divisible by 5, instead say "buzz"
    Otherwise say the number

Sample run and outoput of this function:

*Main> fizzbuzz 15
"1 2 fizz 4 buzz fizz 7 8 fizz buzz 11 fizz 13 14 fizzbuzz"

Test your solution at http://codecheck.it/files/1808270644exxn4wmxe7wi57mqlclneit03Links to an external site.
#lang racket
(provide fizzbuzz)

(define (fizzbuzz n)
  (fizzbuzz-helper 1 n ""))

(define (fizzbuzz-helper i n acc)
    (cond
        ((> i n) acc)  ; Base case
    (else
    (let ((result 
            (cond
              ((and (zero? (remainder i 3)) (zero? (remainder i 5))) "fizzbuzz") ;; set the condition 
              ((zero? (remainder i 3)) "fizz")
              ((zero? (remainder i 5)) "buzz")
            (else (number->string i)))))
      (fizzbuzz-helper (+ i 1) n (string-append acc result (if (= i n) " " " ")))) ; Recursive
      )
    ))


; From: text-adventure.rkt

1. Add a player parameter to the one-turn function.
  The head of the list should be the player's name.
  The tail of the list should be a list of items in the player's possession.
2. Change the monster battle so that players only win if they have found a weapon before attacking the monster.



