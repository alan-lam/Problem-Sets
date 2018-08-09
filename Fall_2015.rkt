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

; PROBLEM 2 (*modified) [hard/easy]
; The Space Boogers are throwing a surprise intergalactic karaoke party for Darth Vader!
; But poor Vady’s condition only allows him to sing no* set of double letters per breath;
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

; PROBLEM 6 (*modified) [hard]
; The Space Boogers accidentally crashed the S.S. Butter into the Sparksonian Museum and rotated
; all the paintings in the process.  Help them restore the paintings to their upright positions before
; the museum opens!  Given a painting represented by a (3x3)* 2D square array (where width equals
; height), a number of times to rotate, and a direction to rotate, return the 2D array representing the
; upright picture.

; The direction will either be ‘L’ for left or ‘R’ for right.

; Examples:  
; [[2, 3, 4],          [[9, 1, 2],
;  [1, 5, 3],           [0, 5, 3],
;  [9, 0, 1]], 3, L  => [1, 3, 4]]

#| Help: https://stackoverflow.com/questions/23177388/rotate-a-list-of-lists
   It didn't occur to me that I could map the "first" function |#

(define (makeMonaSmile painting numRotates direction)
  (if (= (modulo numRotates 4) 0)
      painting
      (if (equal? direction "L")
          (makeMonaSmile (rotateLeft painting) (- numRotates 1) direction)
          (makeMonaSmile (rotateRight painting) (- numRotates 1) direction))))

(define (rotateRight painting)
  (append (list (map first (reverse painting))) (list (map second (reverse painting))) (list (map third (reverse painting)))))

(define (rotateLeft painting)
  (append (list (map third painting)) (list (map second painting)) (list (map first painting))))

(module+ test
  (check-equal? (makeMonaSmile (list (list 2 3 4) (list 1 5 3) (list 9 0 1)) 3 "L")
                (list (list 9 1 2) (list 0 5 3) (list 1 3 4))))

(module+ test
  (check-equal? (makeMonaSmile (list (list 4 9 8) (list 3 0 5) (list 1 2 0)) 7 "R")
                (list (list 8 5 0) (list 9 0 2) (list 4 3 1))))

; PROBLEM 7 [hard] (halp!!!!!)
; The Space Boogers are visiting the Death Star! They want to start a new prank trend among 
; Stormtroopers to only use adherent sentences. In an adherent sentence, the nth word must differ 
; from the (n-1)th word by exactly one letter, ignoring case. All words in the sentence must have the 
; same length. Given a sentence, verify whether the sentence is adherent or not. 

; Examples:
; “Pot hot hog dog dot” =>  true
; “Pot hot dog fog dot” =>  false

; (define (weHaveCookies sentence))

; PROBLEM 8 [hard] (halp!!!!!)
; In the midst of war, Sparkla has figured out how to control the clouds by labelling them with words.
; They need to show their military might by casting a rainbow over enemy territory.
; In order to do so, they must find a pair of anagram clouds.

; Given an array of Strings, check if there exists at least one pair of anagrams in the array.

; Examples:
; [“cat”, “dog”, “act”] => true
; [“cat”, “dog”, “bird”] => false

; (define (thisIsSparrrklaaa words))

; PROBLEM 9 (*modified) [hard]
; Navigator Wartha Umplebom has been separated from her Space Boogers!
; Luckily she still has her potadio (potato radio).
; She wants to contact her Space Boogers, but due to cosmic rays or whatever,
; messages sent are distorted such that every other alphabetic character is shifted such that ‘a’ -> ‘b’,
; ‘b’->’c’, ... , ‘z’->’a’, starting from the second alphabetic character in the string.
; Given Navigator Umplebom’s intended message, return the message the Space Boogers hear.

; Preserve case so that ‘A’ -> ‘B’, etc.

; Examples:
; “Aah, the atmosphere!” => “Abh, uhf aumpsqhfrf!”
; “He’s dead, Jim.” => “Hf’s eebd, Kin.”

(define (pptbdjo message)
  (if (empty? (string->list message))
      empty
      (if (= (length (string->list message)) 1)
          message
          (string-join (flatten (cons (list (make-string 1 (first (string->list message))) (shift (second (string->list message)))) (pptbdjo (list->string (rest (rest (string->list message))))))) ""))))

(define (shift c)
  (if (regexp-match? #rx"[a-y|A-Y]" (make-string 1 c))
      (make-string 1 (integer->char (+ (char->integer c) 1)))
      (if (regexp-match? #rx"[z|Z]" (make-string 1 c))
          (make-string 1 (integer->char (- (char->integer c) 25)))
          (make-string 1 c))))

(module+ test
  (check-equal? (pptbdjo "Aah, the atmosphere!")
                "Abh, uhf btnotpiese!")#|*modified|#
  (check-equal? (pptbdjo "He's dead, Jim.")
                "Hf't eebd, Kin.")#|*modified|#)
