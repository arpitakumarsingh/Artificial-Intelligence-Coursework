; Arpita Kumar Amitesh Singh
; W1412594
; COEN 266
; Assignment 1

;Takes a list comprised of numerical values and returns the largest one.
(define (find-biggest alist)
  (cond ((null? alist) alist)  ;check if input is null return input.
        ((null? (cdr alist)) (car alist)) ; Check if there is only one element, return the single element.
        ((< (car alist) (cadr alist)) (find-biggest (cdr alist))) ;Check if 1st element is smaller than 2nd element, make recursive call to the function
        (#t (find-biggest (cons (car alist) (cdr (cdr alist)))))))

; Takes two integer parameters: a starting and an ending value. Prints all integers between those two number (inclusive).
(define (count-from n m)
  (cond ((equal? m 0) 0)
        ((<= n m) (cons n (count-from (+ n 1) m)))
        (#t '())))

; Takes two parameters: an index (position) and a list. Returns the item in the list corresponding to the index specified.
(define (nth-item i alist)
  (cond ((null? alist) alist)
        ((equal? i 1) (car alist))
        (#t (nth-item (- i 1) (cdr alist)))))

; Takes three parameters: an index (position), a list, and a value to substitute into the list.
; Returns the list, with the item at the specified index replaced with the new value.
(define (replace-nth-item i alist n)
  (cond ((equal? i 1) (cons n (cdr alist)))
        (#t (cons(car alist) (replace-nth-item (- i 1) (cdr alist) n)))))

; Takes a single parameter: a list.
; Returns a boolean value indicating if that list is in a (numerically) sorted order.
(define (sorted? alist)
  (cond ((null? alist) #t)
        ((= 1 (count-items alist)) #t)
        ((> (car (cdr alist)) (car alist)) (sorted? (cdr alist)))
        (#t #f)))

(define (count-items alist)
  (cond ((null? alist) 0)
        (#t (+ 1 (count-items (cdr alist))))))


; Takes two parameters: an action and a state.
; Returns the updated state that will result by following that action.
; A state is a list of three elements: an X position, a Y position, and
; a direction (N, S, E, or W). An action is one of the following strings:
; STAY, MOVE-1, MOVE-2, MOVE-3, TURN-LEFT, TURN-RIGHT, or TURN- AROUND.
; Assume that all moves are forward, relative to the current direction.
(define (dir-val dirlist)
  (cond ((null? dirlist) display "Enter correct direction")
        (#t (cdr(cdr dirlist)))))

(define (apply-action dirlist action)
  (cond ((null? action) display "Action is empty")
        ((null? dirlist) display "Direction is empty")
        ((equal? action "STAY") (list (+ (car dirlist) 0) (+ (car (cdr dirlist)) 0) (car (dir-val dirlist))))
        ((equal? action "MOVE-1") (cond ((equal? (car (dir-val dirlist)) 'N) (list  (car dirlist) (+ (car (cdr dirlist)) 1) (car (dir-val dirlist))))
                                        ((equal? (car (dir-val dirlist)) 'S) (list (car dirlist) (- (car (cdr dirlist)) 1) (car (dir-val dirlist))))
                                        ((equal? (car (dir-val dirlist)) 'E) (list (+ (car dirlist) 1) (car (cdr dirlist)) (car (dir-val dirlist))))
                                        ((equal? (car (dir-val dirlist)) 'W) (list (- (car dirlist) 1) (car (cdr dirlist)) (car (dir-val dirlist))))))
        ((equal? action "MOVE-2") (cond ((equal? (car (dir-val dirlist)) 'N) (list  (car dirlist) (+ (car (cdr dirlist)) 2) (car (dir-val dirlist))))
                                        ((equal? (car (dir-val dirlist)) 'S) (list (car dirlist) (- (car (cdr dirlist)) 2) (car (dir-val dirlist))))
                                        ((equal? (car (dir-val dirlist)) 'E) (list (+ (car dirlist) 2) (car (cdr dirlist)) (car (dir-val dirlist))))
                                        ((equal? (car (dir-val dirlist)) 'W) (list (- (car dirlist) 2) (car (cdr dirlist)) (car (dir-val dirlist))))))
        ((equal? action "MOVE-3") (cond ((equal? (car (dir-val dirlist)) 'N) (list  (car dirlist) (+ (car (cdr dirlist)) 3) (car (dir-val dirlist))))
                                        ((equal? (car (dir-val dirlist)) 'S) (list (car dirlist) (- (car (cdr dirlist)) 3) (car (dir-val dirlist))))
                                        ((equal? (car (dir-val dirlist)) 'E) (list (+ (car dirlist) 3) (car (cdr dirlist)) (car (dir-val dirlist))))
                                        ((equal? (car (dir-val dirlist)) 'W) (list (- (car dirlist) 3) (car (cdr dirlist)) (car (dir-val dirlist))))))
        ((equal? action "TURN-LEFT") (cond ((equal? (car (dir-val dirlist)) 'N) (list  (car dirlist) (car (cdr dirlist)) 'W ))
                                           ((equal? (car (dir-val dirlist)) 'S) (list (car dirlist) (car (cdr dirlist)) 'E ))
                                           ((equal? (car (dir-val dirlist)) 'E) (list (car dirlist) (car (cdr dirlist)) 'N ))
                                           ((equal? (car (dir-val dirlist)) 'W) (list (car dirlist) (car (cdr dirlist)) 'S))))
        ((equal? action "TURN-RIGHT") (cond ((equal? (car (dir-val dirlist)) 'N) (list  (car dirlist) (car (cdr dirlist)) 'E ))
                                            ((equal? (car (dir-val dirlist)) 'S) (list (car dirlist) (car (cdr dirlist)) 'W ))
                                            ((equal? (car (dir-val dirlist)) 'E) (list (car dirlist) (car (cdr dirlist)) 'S ))
                                            ((equal? (car (dir-val dirlist)) 'W) (list (car dirlist) (car (cdr dirlist)) 'N))))
        ((equal? action "TURN-AROUND") (cond ((equal? (car (dir-val dirlist)) 'N) (list  (car dirlist) (car (cdr dirlist)) 'S ))
                                             ((equal? (car (dir-val dirlist)) 'S) (list (car dirlist) (car (cdr dirlist)) 'N ))
                                             ((equal? (car (dir-val dirlist)) 'E) (list (car dirlist) (car (cdr dirlist)) 'W ))
                                             ((equal? (car (dir-val dirlist)) 'W) (list (car dirlist) (car (cdr dirlist)) 'E ))))))

; Takes three parameters: a percept (a la AgentWorld), an X coordinate, and a Y coordinate.
; Assuming that the agent is in location 0,0 and is facing north, returns the element in the specified location.
; Here if "percept" is provided from command-line,
; that will be taken as input otherwise the value defined for percept below will be taken as input.
(define percept '((empty empty empty)
     (empty (vegetation 2 45) empty empty empty)
     ((vegetation 3 150) empty empty empty empty empty barrier)
     (barrier empty empty empty empty empty empty barrier barrier)
     (barrier barrier empty (vegetation 4 200) empty empty empty
      (vegetation 1 125) barrier barrier barrier)))

(define (get-location percept x y)
  (nth-item (+ x (+ y 1)) (nth-item y percept)))

; Takes two lists of integers, each assumed to be in ascending order.
; Returns a single list comprised of the values from both lists in ascending order.
(define (merge-ordered-lists alist1 alist2)
  (cond ((null? alist1) alist2)
        ((null? alist2) alist1)
        ((< (car alist1)(car alist2))(cons (car alist1)(merge-ordered-lists (cdr alist1) alist2)))
        (#t (cons (car alist2) (merge-ordered-lists alist1 (cdr alist2))))))

; Takes one parameter, a list of integer values.
; Returns a list with the same values, sorted in increasing numerical order.
; Merge sort will operate by splitting the list in half (+/- 1 element),
; sorting each of those lists (recursively, using merge-sort),
; and then merging the two sorted lists.
(define (split-list alist)
  (let ((count (count-items alist))) (cond ((= count 0) (list alist alist))
                                           ((= count 1) (list alist '()))
                                           (#t (let ((c (split-list (cdr (cdr alist)))))
                                                    (list (cons (car alist) (car c))
                                                            (cons (car (cdr alist)) (car (cdr c)))))))))

(define (merge-sort alist)
  (cond ((null? alist) '())
        ((= 1 (count-items alist)) alist)
        ((= 2 (count-items alist)) (merge-ordered-lists (list (car alist)) (cdr alist)))
        (#t (merge-ordered-lists (merge-sort (car (split-list alist)))
                                 (merge-sort (car (cdr (split-list alist))))))))
