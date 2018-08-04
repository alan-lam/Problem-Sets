#lang racket
(require rackunit)

; PROBLEM 1
; Pilot Warrick Flerpaderp is applying to get his flying license, but must pass TWO of the three following criteria:
;  - Weigh under 98.5 Sugarthian pounds
;  - Be at least 24 in Unicorn years
;  - Receive a ‘C’ or above on the flying test (possible grades are  ‘A’, ‘B’, ‘C’, ‘D’, and ‘F’)
; Given Pilot Flerpaderp’s weight, age, and test grade, return true if he should receive his flying license.  Else, return false.

; Examples:  
; 90.7, 16, "B" => true  
; 97.6, 22, "F" => false  
; 140.6, 37, "A" => true

(define (canFlyLikeAGSix weight age grade)
  (or
   (and (< weight 98.5) (> age 23))
   (and (< weight 98.5) (checkGrade grade))
   (and (> age 23) (checkGrade grade))
  ))

(define (checkGrade grade)
  (or
   (equal? grade "A")
   (equal? grade "B")
   (equal? grade "C")
   ))

(module+ test
  (check-equal? (canFlyLikeAGSix 90.7 16 "B")
                #t)
  (check-equal? (canFlyLikeAGSix 97.6 22 "F")
                #f)
  (check-equal? (canFlyLikeAGSix 140.6 37 "A")
                #t))

; PROBLEM 2 (modified)
; The Space Boogers are throwing a surprise intergalactic karaoke party for Darth Vader!
; But poor Vady’s condition only allows him to sing no set of double letters per breath;
; otherwise, he’ll lose his voice. For example, “ll” in “hello” is a set of double letters.
; Any given String should be able to be sung in one breath, unless Vady loses his voice by means of a set of double letters.
;Find out if a given String will cause the Sith Lord to lose his voice!

; Examples:  
; Hello, goodbye => true  
; Livin’ on a prayer => false  

(define (soYouThinkYouCanSing words)
  (if (> (length (string->list words)) 1)
      (if (equal? (first (string->list words)) (second (string->list words)))
          #t
          (soYouThinkYouCanSing (list->string (rest (string->list words)))))
      #f))

(module+ test
  (check-equal? (soYouThinkYouCanSing "Hello, goodbye")
                #t)
  (check-equal? (soYouThinkYouCanSing "Livin' on a prayer")
                #f))
