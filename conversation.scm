;
; conversation.scm
;
; Author: Taylor Hoss
; Student ID: x432z869
; Description: A program in scheme similar to to Wiezenbaum's "psychiatrist" 
;              program that reads input from the user and uses pattern matching
;              to come up with a reasonable response.
; Main function: student-advisor
; Sample input: hello, how are you, register, psychology, freshman, goodbye, exit


;main function

(define (student-advisor)
   (define (repeat)
      (let ((input (read-list)))                      ;main loop for accepting input
         (if (not (equal? input '(exit)))
            (begin
               (displayln (respond-to-input input))
               (repeat)
            )
            (display "Thanks for using the student advisor. Goodbye!\n\n")
         )
      )
   )
   (set! *random-state* (random-state-from-platform))            ;reseeding RNG
   (display "\nWelcome to the student advisor!\n")
   (display "(To leave the conversation at any time, enter EXIT)\n\n")
   (displayln "I can help you register for classes or get you in touch with a faculty advisor.")
   (let ((input (read-list)))
      (cond ((equal? '() input)
                    (displayln "Please enter your response."))
            ((not (equal? input '(exit)))
                    (displayln (respond-to-input input)))
            (else (display "Hope to see you again. Goodbye!\n\n"))
      )
      (cond ((not (equal? input '(exit))) (repeat)))
   )
)
            

;function that takes in the input from the user and uses the response-rules
;and the match functions to come up with a suitable answer

(define (respond-to-input fact)

   ;iterates through sets of rules/responses in response-list
   (define (iter pat fact)

      ;matches to rule and returns response
      (define (setcheck pat fact)

         ;matches entire pattern to input

         (define (check pat fact)

            ;matches individual pattern character to input
            (define (match pat fact)
               (cond ((and (null? pat) (null? fact))                    #t)
                     ((or (and (null? pat) (not (null? fact)))
                          (and (null? fact) (not (null? pat)))
                      )                                                 #f)
                     ((equal? pat '(*))                                 #t)
                     ((equal? pat (list (car fact)))                    #t)
                     ((not (equal? pat (list (car fact)))) (match pat (cdr fact)))
               ) 
            );end match

            (if (null? pat)
                (match pat fact)
                (if (match (list (car pat)) fact)
                    (if (null? (cdr pat))
                        #t
                        (check (cdr pat) fact)
                    )
                    #f
                )
            )
         );end check
   
         (if (null? pat)
             #f
             (if (check (car pat) fact)
                 #t
                 #f
             )
          )
      );end setcheck

      (if (setcheck (car pat) fact)
          (pick (cdr (car pat)))           ;pick from responses
          (if (null? (cdr pat))            ;if there are no more patterns
             '(Fact not caught by pattern in iter)  ;then no pattern match (error)
              (iter (cdr pat) fact)        ;otherwise check next pattern
          )
      )
   );end iter

   ;send the response back as a string to be displayed
   (list-of-symbols->string (iter response-rules fact))
)


;patterns for matching responses, car of the embedded list is the pattern
;to match, the car of the embedded list are the responses.

(define response-rules
   '((()
      (Please enter your response.))
     ((* computer science *)
      (Cool! We'll get you in a few software classes. What class standing are you?)
      (Great! I'll set you up with software classes. What's your class standing?))
     ((* computer engineering *)
      (Cool! We'll get you in a few hardware classes. What class standing are you?)
      (Great! I'll set you up with hardware classes. What's your class standing?))
     ((* political science *)
      (Cool! We'll get you in a few government classes. What class standing are you?)
      (Great! I'll set you up with government classes. What's your class standing?))
     ((* science *)
      (Cool! We'll get you in a few science classes. What class standing are you?)
      (Great! I'll set you up with science classes. What's your class standing?))
     ((* physics *)
      (Cool! We'll get you in a few physics classes. What class standing are you?)
      (Great! I'll set you up with physics classes. What's your class standing?))
     ((* chemistry *)
      (Cool! We'll get you in a few chemistry classes. What class standing are you?)
      (Great! I'll set you up with chemistry classes. What's your class standing?))
     ((* biology *)
      (Cool! We'll get you in a few biology classes. What class standing are you?)
      (Great! I'll set you up with biology classes. What's your class standing?))
     ((* health *)
      (Cool! We'll get you in a few health classes. What class standing are you?)
      (Great! I'll set you up with health classes. What's your class standing?))
     ((* psychology *)
      (Cool! We'll get you in a few psychology classes. What class standing are you?)
      (Great! I'll set you up with psychology classes. What's your class standing?))
     ((* sociology *)
      (Cool! We'll get you in a few sociology classes. What class standing are you?)
      (Great! I'll set you up with sociology classes. What's your class standing?))
     ((* art *)
      (Cool! We'll get you in a few art classes. What class standing are you?)
      (Great! I'll set you up with art classes. What's your class standing?))
     ((* design *)
      (Cool! We'll get you in a few design classes. What class standing are you?)
      (Great! I'll set you up with design classes. What's your class standing?))
     ((* english *)
      (Cool! We'll get you in a few english classes. What class standing are you?)
      (Great! I'll set you up with english classes. What's your class standing?))
     ((* writing *)
      (Cool! We'll get you in a few writing classes. What class standing are you?)
      (Great! I'll set you up with writing classes. What's your class standing?))
     ((* engineering *)
      (Cool! We'll get you in a few engineering classes. What class standing are you?)
      (Great! I'll set you up with engineering classes. What's your class standing?))
     ((* freshman *)
      (Newbie huh? You'll like it here. I'll just set you up with some gen eds and you'll be ready to go. There you are. Have a nice day!))
     ((* sophomore *)
      (Ah, made it through the first year it seems, good for you. I'll set you up with a few harder classes and you'll be on your way. Here you are. Have a nice day!))
     ((* junior *)
      (Ah, an experienced student. Looks like you'll be taking a few four hundred level classes this semester. Good luck. Have a nice day!))
     ((* senior *)
      (Finally about to graduate are we? Hopefully you've gotten most of the requirements down. It seems you do, this will be easy. I'll sign you up for the few straggler classes. All done. Have a nice day!))
     ((* register *)
      (Alright, lets get you some classes. What's your major?)
      (I can do that. What major are you going for?)
      (Okay. For what major?))
     ((* class *)
      (Alright, lets get you some classes. What's your major?)
      (I can do that. What major are you going for?)
      (Okay. For what major?))
     ((* classes *)
      (Alright, lets get you some classes. What's your major?)
      (I can do that. What major are you going for?)
      (Okay. For what major?))
     ((* touch *)
      (Alright, I'll get you in touch with your faculty advisor after we get you a basic outline of classes. What's your major?)
      (I can definitely do that, but first let's get you an outline of classes. What major are you going for?))
     ((* faculty *)
      (Alright, I'll get you in touch with your faculty advisor after we get you a basic outline of classes. What's your major?)
      (I can definitely do that, but first let's get you an outline of classes. What major are you going for?))
     ((* advisor *)
      (Alright, I'll get you in touch with your faculty advisor after we get you a basic outline of classes. What's your major?)
      (I can definitely do that, but first let's get you an outline of classes. What major are you going for?)) 
     ((* hello *)
      (Hi! What can I do for you?)
      (Hello! What are you here for today?))
     ((* hi *)
      (Hi! What can I do for you?)
      (Hello! What are you here for today?))
     ((* howdy *)
      (Hi! What can I do for you?)
      (Hello! What are you here for today?))
     ((* goodbye *)
      (Goodbye! Remember to type EXIT to leave the conversation.)
      (Bye bye! Remember to type EXIT to leave the conversation.)
      (Bye now! Remember to type EXIT to leave the conversation.))
     ((* bye *)
      (Goodbye! Remember to type EXIT to leave the conversation.)
      (Bye bye! Remember to type EXIT to leave the conversation.)
      (Bye now! Remember to type EXIT to leave the conversation.))
     ((* see you later *)
      (Goodbye! Remember to type EXIT to leave the conversation.)
      (Bye bye! Remember to type EXIT to leave the conversation.)
      (Bye now! Remember to type EXIT to leave the conversation.))
     ((* have a nice day *)
      (Goodbye! Remember to type EXIT to leave the conversation.)
      (Bye bye! Remember to type EXIT to leave the conversation.)
      (Bye now! Remember to type EXIT to leave the conversation.))
     ((* you too *)
      (Goodbye! Remember to type EXIT to leave the conversation.)
      (Bye bye! Remember to type EXIT to leave the conversation.)
      (Bye now! Remember to type EXIT to leave the conversation.))
     ((* how are you *)
      (I'm good. Thank you for asking. How are you?)
      (Good, thanks! How are you?))
     ((* not good *)
      (Sorry to hear that. What can I do for you today?)
      (Thats unfortunate. What will we be doing today?))
     ((* not great *)
      (Sorry to hear that. What can I do for you today?)
      (Thats unfortunate. What will we be doing today?))
     ((* good *)
      (Great to hear! What can I do for you today?)
      (Always good to hear that! What will we be doing today?))
     ((* fine *)
      (Great to hear! What can I do for you today?)
      (Always good to hear that! What will we be doing today?))
     ((* great *)
      (Great to hear! What can I do for you today?)
      (Always good to hear that! What will we be doing today?))
     ((* well *)
      (Great to hear! What can I do for you today?)
      (Always good to hear that! What will we be doing today?))
     ((* bad *)
      (Sorry to hear that. What can I do for you today?)
      (Thats unfortunate. What will we be doing today?))
     ((* terrible *)
      (Sorry to hear that. What can I do for you today?)
      (Thats unfortunate. What will we be doing today?))
     ((*)
      (I'm not sure I understand what you're saying.)
      (I'm not sure what that is.)
      (What was that?)
      (Come again?))
    )
)

;display parameter followed by newline followed by prompt for user input

(define (displayln x)
   (display x)
   (newline)
   (display ">> ")
)


;returns text entered by user as list of symbols with punctuation removed,
;and everything converted to lowercase.

(define (read-list)
   (let ((list-of-strings
          (string-tokenize (string-downcase (remove-punctuation (readline))))
        ))
     (map string->symbol list-of-strings)
   )
)


;return the string formed by appending each of the strings in the parameter
;together, with spaces between them.

(define (make-string-with-spaces lst-of-strs)
   (if (null? lst-of-strs)
       ""
       (string-append (car lst-of-strs)
                      " "
                      (make-string-with-spaces (cdr lst-of-strs))
       )
   )
)


;return the string formed from a list of symbols by converting each symbol
;to a string and adding spaces between them

(define (list-of-symbols->string lst)
   (make-string-with-spaces (map symbol->string lst))
)


;return the list which has the elements from its parameter, but all at the 
;top level rather than in sub-lists

(define (flatten lst)
   (cond ((null? lst) '())
         ((list? (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
         (else (append (list (car lst)) (flatten (cdr lst))))
   )
)


;replaces the parameter with a space

(define (replace-with-space str ch)
   (let ((str-list (string->list str)))
      (define (next lst)
         (if (null? lst)
             (quote "")
             (if (char=? (car lst) ch)
                 (string-append
                    (list->string (list #\space))
                    (next (cdr lst))
                 )
                 (string-append
                    (list->string (list (car lst)))
                    (next (cdr lst))
                 )
             )
         )
      )
      (next str-list)
   )
)


;removes punctuation and replaces with space

(define (remove-punctuation str)
   (let ((punctuation (string->list ".,;:`!?#-()\\\"")))
      (define (next punct-list tmp-str)
         (if (null? punct-list)
             tmp-str
             (next (cdr punct-list)
                   (replace-with-space tmp-str (car punct-list))
             )
         )
      )
      (next punctuation str)
   )
)


;picks a random element from a list

(define (pick lst)
   (list-ref lst (random (length lst)))
)



