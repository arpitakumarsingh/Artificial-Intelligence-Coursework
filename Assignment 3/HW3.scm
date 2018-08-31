; Arpita Kumar Amitesh Singh
; W1412594
; COEN 266
; Assignment 3

;-------------------------------------------------------------------------------
; Returns true if item is present in list or sublist
;-------------------------------------------------------------------------------
(define check
 (lambda(item lis)
  (cond((null? lis) #f)
       (else(if(pair? (car lis))
               (if(check item (car lis)) #t (check item (cdr lis)))
               (if(equal? item (car lis)) #t (check item (cdr lis))))))))

;-------------------------------------------------------------------------------
;Returns list or sublist in which the element is present
;-------------------------------------------------------------------------------
(define item-display
 (lambda(item lis)
  (cond((null? lis) #f)
       (else(if(pair? (car lis))
               (if(item-display item (car lis)) (car lis) (item-display item (cdr lis)))
               (if(equal? item (car lis)) (car lis) (item-display item (cdr lis))))))))

;-------------------------------------------------------------------------------
;Checks if the list has "NOT" in car position
;-------------------------------------------------------------------------------
(define (is-not? expression)
  (equal? (car expression) 'NOT))

;-------------------------------------------------------------------------------
;Checks if current element of input is list or not, if its a list compare the
;cdr element with target list. if not a list compare car of element with target
; list. If not found adds to output list.
;-------------------------------------------------------------------------------
(define (compare alist1 alist2 alist3)
        (cond ((null? alist1)
                        alist3)
                 ((list? (car alist1))
                        (if (equal? (check (car (cdr (car alist1))) alist2) #f)
                                (compare (cdr alist1) alist2 (append alist3 (list(car alist1))))
                                (compare (cdr alist1) alist2 alist3)))
                 ((equal? (list? (car alist1)) #f)
                       (compare-helper alist1 alist2 alist3))
                (#t
                        (compare (cdr alist1) alist2 alist3))))

;-------------------------------------------------------------------------------
;Helper function for compare.
;-------------------------------------------------------------------------------
(define (compare-helper alist1 alist2 alist3)
            (cond ((equal? (check (car alist1) alist2) #f)
                  (compare (cdr alist1) alist2 (append alist3 (list(car alist1)))))
                  ((check (car alist1) alist2)
                      (if (list? (item-display (car alist1) alist2))
                            (if (equal? (is-not? (item-display (car alist1) alist2)) #f)
                                  (compare (cdr alist1) alist2 (append alist3 (list (car alist1)))) alist3)
                            (compare (cdr alist1) alist2 (append alist3 (list (car alist1))))))))

;-------------------------------------------------------------------------------
;Function to check if the lists are contradicting.
;-------------------------------------------------------------------------------
(define (contrafunc alist1 alist2)
        (if (and (equal? (length alist1) 1) (equal? (length alist2) 1))
        (cond ((list? (car alist1))
                (if (and (and (is-not? (car alist1))
                              (equal? (length (car alist1)) 2))
                         (equal? (car (cdr (car alist1))) (car alist2)))
                            #t))
                    ((equal? (list? (car alist1)) #f)
                        (if (and (and (is-not? (car alist2))
                            (equal? (length (car alist2)) 2))
                            (equal? (car alist1) (car (cdr (car alist2)))))
                            #t)))
                #f))

;-------------------------------------------------------------------------------
;Helper function for common. To check if there is at least one element of first
;list in second list. If present adds to output list.
;-------------------------------------------------------------------------------
(define (common-helper alist1 alist2 alist3)
        (cond ((null? alist1) alist3)
              ((list? (car alist1))
                (if (check (car (cdr (car alist1))) alist2)
                  (common-helper (cdr alist1) alist2 (append (car alist1) alist3))
                  (common-helper (cdr alist1) alist2 alist3)))
              ((equal? (list? (car alist1)) #f)
                   (if (check (car alist1) alist2)
                       (common-helper (cdr alist1) alist2 (append (list (car alist1)) alist3))
                       (common-helper (cdr alist1) alist2 alist3)))))

;-------------------------------------------------------------------------------
;Calls helper function above.
;-------------------------------------------------------------------------------
(define (common alist1 alist2)
  (common-helper alist1 alist2 '()))

;-------------------------------------------------------------------------------
;Helper function for resolve
;-------------------------------------------------------------------------------
(define (resolve-helper alist1 alist2)
        (cond ((null? alist1) #f)
          (#t (compare alist1 alist2 '()))))

;-------------------------------------------------------------------------------
;Checks if there is any common element between two lists, if empty list returned by common function, retuns false.
;if Compare returns empth list, checks if its a contradiction and prints contradiction.
;Otherwise prints resolved list.
(define (resolve alist1 alist2)
    (cond ((null? alist1)
       #f)
      ((null? (common alist1 alist2))
         #f)
        ((null? (compare alist2 alist1 (resolve-helper alist1 alist2)))
           (if (contrafunc alist1 alist2)
              "CONTRADICTION"
            #f))
            (#t
              (compare alist2 alist1 (resolve-helper alist1 alist2)))))

;-------------------------------------------------------------------------------
;End of Basic Assignment
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;Defines knowledge base
(define KB '())

;-------------------------------------------------------------------------------
;Tell function, reads list from user and adds it to KB
(define (tell alist1)
       (set! KB (append alist1 KB)) "OK")

;-------------------------------------------------------------------------------
;Ask function, reads user input and calls resolve function to resolve arguments.
;if resolved, returns true otherwise returns Unknown
;-------------------------------------------------------------------------------
(define (ask alist2)
    (cond ((resolve alist2 KB) #t)
           (#t "UNKNOWN")))

;-------------------------------------------------------------------------------
;End
;-------------------------------------------------------------------------------
