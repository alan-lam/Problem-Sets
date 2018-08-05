#lang racket
(require rackunit)

; PROBLEM 1 [easy]
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

; PROBLEM 2 (modified) [easy]
; The Space Boogers are throwing a surprise intergalactic karaoke party for Darth Vader!
; But poor Vady’s condition only allows him to sing no set of double letters per breath;
; otherwise, he’ll lose his voice. For example, “ll” in “hello” is a set of double letters.
; Any given String should be able to be sung in one breath, unless Vady loses his voice by means of a set of double letters.
; Find out if a given String will cause the Sith Lord to lose his voice!

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

; PROBLEM 3 [easy]
; First Mate Wendy Fuzzlebutt fell asleep while eating a caramel apple.
; Now her face is stuck so that the only vowel she can make is ‘u.’
; Given a String containing what First Mate Fuzzlebutt wants to say, return what she really says.

; Preserve the case of any replaced vowels. Vowels include ‘a’, ‘e’, ‘i’, ‘o’, ‘u’, and ‘y’.

; Examples:
; “I am an AWESOME unicorn!” => “U um un UWUSUMU unucurn!”
; “Je suis allergique au citron” => “Ju suus ullurguquu uu cutrun”

(define (wuuUsMu uMu)
  (list->string (map checkVowel (string->list uMu))))

(define (checkVowel c)
  (if (member c (list #\a #\e #\i #\o #\u #\y))
      #\u
      (if (member c (list #\A #\E #\I #\O #\U #\Y))
          #\U
          c)))

(module+ test
  (check-equal? (wuuUsMu "I am an AWESOME unicorn!")
                "U um un UWUSUMU unucurn!")
  (check-equal? (wuuUsMu "Je suis allergique au citron")
                "Ju suus ullurguquu uu cutrun"))

; PROBLEM 4 [hard]
; Navigator Wartha Umplebom loves eggs! But eggs are hard to find. They are hidden in many places.
; She needs to identify the positions of the eggs so that she can eat them later.
; Given a String, identify the indices of all “eggs” and output an int array containing these indices.
; Only the number zero “0”, the lower case letter “o”, and the upper case letter “O” are considered “eggs”.

; Each String is zero indexed, meaning that the first character has index 0, the second character has
; index 1, etc. If there are no eggs, output an empty array.

; Examples:
; 120 => [2]
; I love FOOD => [3, 8, 9]

(define (honeyWhereAreMyEggs eggString)
  (if (regexp-match? #rx"o|O|0" eggString)
      (cons (car (first (regexp-match-positions #rx"o|O|0" eggString))) (honeyWhereAreMyEggs (regexp-replace #rx"o|O|0" eggString "a")))
      '()))
      
(module+ test
  (check-equal? (honeyWhereAreMyEggs "120")
                '(2))
  (check-equal? (honeyWhereAreMyEggs "I love FOOD")
                '(3 8 9)))

; PROBLEM 5 [easy]
; Engineer Wallace Lickalot has decreed that a number is considered “galactastic” if it is a palindrome
; and contains 1 or more zeroes beginning from the center.
; For example, “50505” is not considered galactastic, but “50005” is.

; Given a number, find the next galactastic number that is strictly greater than the given number.

; Examples:
; 98 => 101
; 4556 => 5005
; 64986 => 65056

(define (absolutelyGalactastic num)
  (if (isGalactastic num)
      num
      (absolutelyGalactastic (+ num 1))))

(define (isGalactastic num)
  (if (or (= num 0) (= num 00))
      #t
      (if (equal? (first (string->list (number->string num))) (last (string->list (number->string num))))
          (if (> (length (string->list (number->string num))) 2)
              (isGalactastic (string->number (substring (number->string num) 1 (- (length (string->list (number->string num))) 1))))
              #f)
          #f)))

(module+ test
  (check-equal? (absolutelyGalactastic 98)
                101)
  (check-equal? (absolutelyGalactastic 4556)
                5005)
  (check-equal? (absolutelyGalactastic 64986)
                65056))
