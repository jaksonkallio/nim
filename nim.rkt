;; This file contains excerpts from the textbook Concrete
;; Abstractions: An Introduction to Computer Science Using Scheme, by
;; Max Hailperin, Barbara Kaiser, and Karl Knight, Copyright (c) 1998
;; by the authors. Full text is available for free at
;; http://www.gustavus.edu/+max/concrete-abstractions.html

;; Chapter 6: Compound Data and Data Abstraction

;; 6.2  Nim
(define play-with-turns
  (lambda (game-state player)
    (display-game-state game-state)
    (cond ((over? game-state) 
           (announce-winner player))
          ((equal? player 'human)  
           (play-with-turns (human-move game-state) 'computer))
          ((equal? player 'computer)  
           (play-with-turns (computer-move game-state simple-strategy) 'human))
          (else  
           (error "player wasn't human or computer:" player)))))

(define computer-move
  (lambda (game-state strategy)
    (let ((pile (if (> (size-of-pile game-state 1) 0)
                    1
                    2)))
      (display "I take 1 coin from pile ")
      (display pile)
      (newline)
      (next-game-state game-state (strategy game-state)))))

(define prompt
  (lambda (prompt-string valid? error-string)
    (newline)
    (display prompt-string)
    (newline)
    (let ((input (read)))
      (if (valid? input)
          input
          (begin (display error-string)
                 (prompt prompt-string valid? error-string))))))

(define human-move
  (lambda (game-state)
    (let ((p (prompt "Which pile will you remove from?"
                     (lambda (input)
                       (and (integer? input) (>= input 1) (<= input 2)))
                     "Bad pile number. Try again.")))
      (let ((n (prompt "How many coins do you want to remove?"
                       (lambda (input)
                         (and (integer? input) (> input 0) (>= (size-of-pile game-state p) input)))
                       "Bad number of coins. Try again.")))
        (next-game-state game-state (cons n p))))))

(define over?
  (lambda (game-state)
    (= (total-size game-state) 0)))

(define announce-winner
  (lambda (player)
    (if (equal? player 'human) 
        (display "You lose. Better luck next time.")
        (display "You win. Congratulations."))))

(define nim
  (lambda ()
    (let ((coinflip (random 2))
          (i-val (+ 6 (random 7)))
          (j-val (+ 6 (random 7))))
      (if (= i-val j-val)
          (nim)
          (begin (display "Welcome to the game of Nim.")
                 (newline)
                 (display "I have a number between 0 and 1. Guess?")
                 (let ((chosen-side (prompt ""
                                            (lambda (input)
                                              (and (integer? input) (>= input 0) (<= input 1)))
                                            "Not a valid flip guess.")))
                   (if (= coinflip chosen-side)
                       (begin (display "The number is ")
                              (display coinflip)
                              (display ". The you may go first.")
                              (play-with-turns (make-game-state i-val j-val) 'human))
                       (begin (display "The number is ")
                              (display coinflip)
                              (display ". I will go first.")
                              (play-with-turns (make-game-state i-val j-val) 'computer)))))))))
;; 6.3  Representations and Implementations

;; Sidebar: Game State ADT Implementation

(define make-game-state
  (lambda (n m) (cons n m)))

(define size-of-pile
  (lambda (game-state pile-number)
    (if (= pile-number 1)
        (car game-state)
        (cdr game-state))))

(define remove-coins-from-pile
  (lambda (game-state num-coins pile-number)
    (if (= pile-number 1)
        (make-game-state (- (size-of-pile game-state 1)
                            num-coins) 
                         (size-of-pile game-state 2))
        (make-game-state (size-of-pile game-state 1)
                         (- (size-of-pile game-state 2)
                            num-coins)))))

(define next-game-state
  (lambda (game-state move-instruction)
    (cond ((= (pile move-instruction) 1)
           (make-game-state (- (size-of-pile game-state 1)
                               (car move-instruction)) 
                            (size-of-pile game-state 2)))
          ((= (pile move-instruction) 2)
           (make-game-state (size-of-pile game-state 1)
                            (- (size-of-pile game-state 2)
                               (car move-instruction)))))))

(define display-game-state
  (lambda (game-state)
    (newline)
    (newline)
    (display "    Pile 1: ")
    (display (size-of-pile game-state 1))
    (newline)
    (display "    Pile 2: ")
    (display (size-of-pile game-state 2))
    (newline)
    (newline)))

(define total-size
  (lambda (game-state)
    (+ (size-of-pile game-state 1)
       (size-of-pile game-state 2))))

(define simple-strategy
       (lambda (game-state)
         (if (> (size-of-pile game-state 1) 0) 
             (make-move-instruction 1 1)    ; remove one coin from pile 1
             (make-move-instruction 1 2))))

;; *******************************************************************
;; A Move Instruction ADT (Exercise 6.13, pp. 156-7)

(define make-move-instruction cons)

(define coins car)

(define pile cdr)

;; *******************************************************************
;; 3-pile Nim game state representation
;;
;; When you are ready to move to 3-pile nim, uncomment the constructor
;; and selector below

;; (define make-game-state
;;   (lambda (n m k) (cons k (cons n m))))

;; (define size-of-pile
;;   (lambda (game-state pile-number)
;;     (cond ((= pile-number 3)
;; 	   (car game-state))
;; 	  ((= pile-number 1)
;; 	   (car (cdr game-state)))
;; 	  (else ; pile-number must be 2
;; 	   (cdr (cdr game-state))))))
