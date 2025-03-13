; Part 1)
;; test.bash. Before you run the program, write down your guess of what the output should be.
;; What is the issue?

; the output will be 42

; After running the program, got 666
; The issue is that Bash uses dynamic scoping, so the result x in this case is 666

;; Download object.racket.  Using closures and the box function,
; implement the Employee constructor and the getters/setters for each field of the object.

; (You still may not directly use set!, but you may use the set-val! function FOR THIS ASSIGNMENT ONLY.)

; How could you improve the code to remove this repetition?


#lang racket

;; when the second item to cons is not
;; a list, we have a pair.
;; A "box" function that encapsulates a mutable value using closures
(define (box x)
  (cons
   (λ () x)             ;; Getter function
   (λ (y) (set! x y)))) ;; Setter function (modifies x)

;; Retrieve value from the boxed pair
(define (get-val bx)
  ((car bx)))

;; Set new value in the boxed pair
(define (set-val! bx new-val)
  ((cdr bx) new-val))

;; An employee object is represented as a list of 3 setter-getter pairs
;; Employee constructor using boxed fields
(define (Employee name position salary)
    (list (box name)      ;; Boxed name
        (box position)  ;; position
        (box salary)))  ;; salary

;; Generic getter function to extract the field at index 'n'
(define (get-field emp n)
    (get-val (list-ref emp n)))

;; Generic setter function to modify the field at index 'n'
(define (set-field! emp n new-val)
    (set-val! (list-ref emp n) new-val))

;; Define specific getter and setter functions using the generic ones
(define (get-name emp) (get-field emp 0))
(define (set-name emp new-name) (set-field! emp 0 new-name))

(define (get-position emp) (get-field emp 1))
(define (set-position emp new-pos) (set-field! emp 1 new-pos))

(define (get-salary emp) (get-field emp 2))
(define (set-salary emp new-sal) (set-field! emp 2 new-sal))

;; Test Cases
(define prof (Employee "Austin" "Professor" 99999999999999999))

;; Get initial values
(get-name prof)       ;; Expected: "Austin"
(get-position prof)   ;; Expected: "Professor"
(get-salary prof)     ;; Expected: 99999999999999999

;; Set new values
(set-name prof "Tom the Mighty")
(set-position prof "Master of Time and Space")
(set-salary prof 12345678)

;; Get updated values
(get-name prof)       ; Expected: "Tom the Mighty"
(get-position prof)   ;; Expected: "Master of Time and Space"
(get-salary prof)     ;; Expected: 12345678


;; The getters and setters are repetitive because each one follows the same pattern:
; Getters call get-val
; Setters call set-val!
;; Instead of defining separate get-name, get-position, get-salary,.. manually, I can improve the code by removing redundancy by creating a higher-order function that generates these getters and setters dynamically.
;; Generate them dynamically using make-getter / make-setter. For example, If I add more fields to Employee, just need to update the constructor and define a new getter/setter using make-setter/ make-getter, no need to write redundant functions.