; Arpita Kumar Amitesh Singh
; W1412594
; COEN 266
; Assignment 2

;-------------------------------------------------------------------------------
;Input Adjacency Map
;-------------------------------------------------------------------------------
(define adjacency-map '(
  (Alabama Mississippi Tennessee Georgia Florida)
  (Alaska)
  (Arkansas Texas Oklahoma Missouri Tennessee Mississippi Louisiana)
  (Arizona California Nevada Utah New-Mexico)
  (California Arizona Nevada Oregon)
  (Colorado New-Mexico Utah Wyoming Nebraska Kansas Oklahoma)
  (Connecticut New-York Massachusetts Rhode-Island)
  (Delaware Maryland Pennsylvania New-Jersey)
  (Florida Alabama Georgia)
  (Georgia Florida Alabama Tennessee North-Carolina South-Carolina)
  (Hawaii)
  (Idaho Oregon Washington Montana Wyoming Utah Nevada)
  (Indiana Illinois Michigan Ohio Kentucky)
  (Illinois Missouri Iowa Wisconsin Indiana Kentucky)
  (Iowa Missouri Illinois Wisconsin Minnesota South-Dakota Nebraska)
  (Kansas Colorado Nebraska Missouri Oklahoma)
  (Kentucky Missouri Illinois Indiana Ohio West-Virginia Virginia Tennessee)
  (Louisiana Texas Arkansas Mississippi)
  (Maine New-Hampshire)
  (Maryland Virginia West-Virginia Pennsylvania Delaware)
  (Massachusetts Rhode-Island Connecticut New-York Vermont New-Hampshire)
  (Michigan Wisconsin Indiana Ohio)
  (Minnesota North-Dakota South-Dakota Iowa Wisconsin)
  (Mississippi Louisiana Arkansas Tennessee Alabama)
  (Missouri Oklahoma Kansas Nebraska Iowa Illinois Kentucky Tennessee Arkansas)
  (Montana Idaho Wyoming South-Dakota North-Dakota)
  (Nebraska Colorado Kansas Missouri Iowa South-Dakota Wyoming)
  (Nevada California Arizona Utah Idaho Oregon)
  (New-Hampshire Maine Vermont Massachusetts)
  (New-Jersey Delaware Pennsylvania New-York)
  (New-Mexico Texas Oklahoma Colorado Arizona)
  (New-York Pennsylvania New-Jersey Connecticut Massachusetts Vermont)
  (North-Carolina South-Carolina Georgia Tennessee Virginia)
  (North-Dakota Montana South-Dakota Minnesota)
  (Ohio Michigan Indiana Kentucky West-Virginia Pennsylvania)
  (Oklahoma Texas New-Mexico Colorado Kansas Missouri Arkansas)
  (Oregon Washington Idaho Nevada California)
  (Pennsylvania Ohio West-Virginia Maryland Delaware New-Jersey New-York)
  (Rhode-Island Connecticut Massachusetts)
  (South-Carolina Georgia North-Carolina)
  (South-Dakota Nebraska Iowa Minnesota North-Dakota Montana Wyoming)
  (Tennessee Arkansas Missouri Kentucky Virginia North-Carolina Georgia Alabama Mississippi)
  (Texas New-Mexico Oklahoma Arkansas Louisiana)
  (Utah Nevada Idaho Wyoming Colorado Arizona)
  (Vermont New-York Massachusetts New-Hampshire)
  (Virginia North-Carolina Tennessee Kentucky West-Virginia Maryland)
  (Washington Oregon Idaho)
  (West-Virginia Virginia Kentucky Ohio Pennsylvania Maryland)
  (Wisconsin Minnesota Iowa Illinois Michigan)
  (Wyoming Idaho Montana South-Dakota Nebraska Colorado Utah)
))

;----------------------------------------------------------------------------------------------------------------
;Code to check if first state in input is present in adjacency map and to get the rest of adjacency states to it.
;----------------------------------------------------------------------------------------------------------------

;Gets the index of the list which has value in 1st position as the value given in input 'state'.
(define (getPosition adjacency-map state pos)
  (cond ((null? adjacency-map) '())              ; list was empty
        ((equal? state (car (car adjacency-map))) pos) ; we found it!
        (#t (getPosition (cdr adjacency-map) state (+ 1 pos)))))

; Takes two parameters: an index (position) and a list.
; Returns the item in the list corresponding to the index specified.
(define (nth-item i alist)
   (cond ((null? alist) alist)
           ((equal? i 1) (car alist))
           (#t (nth-item (- i 1) (cdr alist)))))

;Gives the list of adjacent states from adjacency map
(define (adjacent-state adjacency-map state)
  (cdr (nth-item (getPosition adjacency-map state 1) adjacency-map)))

;---------------------------------------------------------------------------------------------------------------
;Code for swapping elements.
;---------------------------------------------------------------------------------------------------------------

; Takes three parameters: an index (position), a list, and a value to substitute into the list.
; Returns the list, with the item at the specified index replaced with the new value.
(define (replace-nth-item i alist n)
   (cond ((zero? i) '(()))
        ((equal? i 1) (cons n (cdr alist)))
         (#t (cons(car alist) (replace-nth-item (- i 1) (cdr alist) n)))))

;Function to copy input list to temp list to use in swap function
(define (copy-list alist1)
  (cond ((null? alist1) alist1)
        ((list? alist1)  (cons (copy-list (car alist1)) (copy-list (cdr alist1))))
        (#t alist1)))

;Function to swap elements at x and y position
(define (swap-elements x y alist)
  (let ((alist2 (copy-list alist)))
      (replace-nth-item y (replace-nth-item x alist (nth-item y alist)) (nth-item x alist2))))

;----------------------------------------------------------------------------------------------------------------
;Check adjacency
;----------------------------------------------------------------------------------------------------------------

;Function to check if state is present in adjacency-list.
(define (list-contains state alist)
        (cond   ((null? alist) #f)
                ((equal? state (car alist)) #t)
                ((null? (cdr alist)) #f)
                (#t (list-contains state (cdr alist)))))

;Function checks if state one and state two are adjacent to each other
(define (is-adjacent? state1 state2)
        (cond ((list-contains state2 (adjacent-state adjacency-map state1)) #t)
                (else #f)))

;----------------------------------------------------------------------------------------------------------------
; Get children
;----------------------------------------------------------------------------------------------------------------

;Function to get the pair of possible combinations of a list. Input will be 2 and say if size of
;state input is 5 then output will be ((1 2) (1 3).....)
(define (combinations k nlst)
  (cond ((zero? k) '(()))
        ((null? nlst) '())
        (#t (append (map (lambda (k-1)
                        (cons (car nlst) k-1))
                      (combinations (- k 1) (cdr nlst)))
                 (combinations k (cdr nlst))))))

; Takes two integer parameters: a starting and an ending value. Prints all integers between those two number (inclusive).
(define (count-from n m)
   (cond ((equal? m 0) 0)
           ((<= n m) (cons n (count-from (+ n 1) m)))
                   (#t '())))

;Helper function to get children
(define (get-children-helper combinations alist)
        (cond ((null? (cdr combinations))
                        (list (append (list (swap-elements (car (car combinations))
                                (car (cdr (car combinations))) alist))
                                (list (car combinations)))))
                (#t (append (list(list (swap-elements (car (car combinations))
                                (car (cdr (car combinations))) alist) (car combinations)))
                                (get-children-helper (cdr combinations) alist)))))

;Function to get children
(define (get-children alist)
        (let ((c (count-from 1 (length (car alist)))) (temp (car alist)))
              (get-children-helper (combinations 2 c) temp)))

;---------------------------------------------------------------------------------------------------------------
;Function to determine if a state is a goal state.--
;---------------------------------------------------------------------------------------------------------------

(define (is-goal-state? alist)
  (cond ((null? (cdr alist)) #t)
        (#t (if (is-adjacent? (car alist) (car (cdr alist)))
                  (is-goal-state? (cdr alist)) #f))))

;---------------------------------------------------------------------------------------------------------------
;DFS - depth-first search algorithm.
;---------------------------------------------------------------------------------------------------------------

;Function to check if the combination already exists, to avoid cycles.
(define (check-frontier visited children-list)
  (cond ((null? (cdr children-list)) #f)
        (#t (if (list-contains (car (car children-list)) visited)
                  (check-frontier visited (cdr children-list)) (car children-list)))))

;Helper function for DFS
(define (dfs-helper frontier visited path)
  (cond ((= (length (car frontier)) 1) (list (car frontier) (car path)))
        ((is-goal-state? (car frontier)) (list (reverse (car frontier)) (if (null? (car path))
                                                    (car path)
                                                    (append path (list (car (cdr frontier)))))))
        (#t (dfs-helper (check-frontier (cons (car frontier) visited)
                      (get-children frontier)) (cons (car frontier) visited)
                        (if (null? (car path))
                              (list (car (cdr frontier)))
                              (append path (list (car (cdr frontier)))))))))

(define (dfs alist)
        (cond ((null? alist) #f)
                ((equal? (length alist) 2) (is-goal-state? alist))
                (#t (dfs-helper (list alist '()) '() '(())))))

;--------------------------------------------------------------------------------------------------------------
;Iterative Deepening DFS search algorithm.
;--------------------------------------------------------------------------------------------------------------

; ID-DFS Helper Function
(define (id-dfs-helper depth limit frontier visited path)
        (cond ((null? frontier) #f)
                ((= (length (car frontier)) 1) (list (car frontier) (car path)))
                ((equal? 0 limit) frontier)
                ((<= depth limit)
                        (cond ((is-goal-state? (car frontier)) (list (car frontier)
                                                (if (null? (car path))
                                                    (car path)
                                                    (append path (list (car (cdr frontier)))))))
                                (#t (id-dfs-helper (+ depth 1) limit
                                        (check-frontier (cons (car frontier) visited) (get-children frontier))
                                                         (cons (car frontier) visited)
                                                         (if (null? (car path))
                                                                (list (car (cdr frontier)))
                                                                  (append path (list (car (cdr frontier)))))))))
                (#t #f)))

(define (id-dfs alist)
        (cond ((null? alist) #f)
                ((equal? (length alist) 2) (is-goal-state? alist))
                (#t (id-dfs-helper 1 (- (length alist) 1) (list alist '()) '() '(())))))
