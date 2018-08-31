; Arpita Kumar Amitesh Singh
; W1412594
; COEN 266
; Assignment 4

;-------------------------------------------------------------------------------
;Variables with default values.
;-------------------------------------------------------------------------------
(define chart (list))
(define veg-types (list))
(define location '(0 0 N))
(define turn-ctr 0)
(define desire "explore")
(define mood "peaceful") ;determines goal of actions
(define target (list)) ;target location to move to
(define forced-move (list)) ;used for getting out of an obstacles way
(define desire-ctr 25) ;number of turns before desire changes

;-------------------------------------------------------------------------------
;Main function.
;-------------------------------------------------------------------------------

(define (initialize-agent) "ok")

(define (choose-action current-energy previous-events percepts)
  (begin (view percepts -1 1)
        (choose current-energy previous-events percepts)
        (cond ((equal? desire "eat")
                    (eat current-energy previous-events percepts)) ;
              ((equal? desire "explore")
                    (explore percepts)) ;
              ((equal? desire "target")
                    (seek-target)) ;
       )))

;-------------------------------------------------------------------------------
; Get the things like vegetation and predator information.
;-------------------------------------------------------------------------------
(define (view percepts x y)
        (cond ((> y 5) "ok")
              ((> x y) (view percepts (- 0 (+ 1 y)) (+ 1 y)))
              ((list? (get-location percepts x y))
                      (begin
                        (set! chart (cons (list (get-location percepts x y)
                                                (x-loc x y)
                                                (y-loc x y))
                                          chart))
                                 (if (equal?
                                        (nth-item 1 (get-location percepts x y))
                                        'vegetation)
                                     (is-new? (get-location percepts x y)))
                        (view percepts (+ 1 x) y)))
              (#t (view percepts (+ 1 x) y))))

;-------------------------------------------------------------------------------
; Convert percept location to co-ordinates.
;-------------------------------------------------------------------------------
(define (x-loc x y)
  (cond ((equal? (nth-item 3 location) 'N) (+ x (nth-item 1 location)))
        ((equal? (nth-item 3 location) 'W) (- (nth-item 1 location) y))
        ((equal? (nth-item 3 location) 'S) (- (nth-item 1 location) x))
        ((equal? (nth-item 3 location) 'E) (+ (nth-item 1 location) y))))

(define (y-loc x y)
  (cond ((equal? (nth-item 3 location) 'N) (+ y (nth-item 2 location)))
        ((equal? (nth-item 3 location) 'W) (+ x (nth-item 2 location)))
        ((equal? (nth-item 3 location) 'S) (- (nth-item 2 location) y))
        ((equal? (nth-item 3 location) 'E) (- (nth-item 2 location) x))))

;-------------------------------------------------------------------------------
; Add vegetation from percept to veg-list.
;-------------------------------------------------------------------------------
(define (is-new? vegetation)
  (if (equal? (present-idx vegetation) 0)
        (set! veg-types (cons vegetation veg-types))
        (if (> (nth-item 3 vegetation)
                (nth-item 3 (nth-item (present-idx vegetation) veg-types)))
            (set! veg-types (replace-nth-item (present-idx vegetation)
                              veg-types vegetation)))))

;-------------------------------------------------------------------------------
; Check if vegetation is already present in the list.
;-------------------------------------------------------------------------------
(define (present-idx vegetation)
  (check-veg vegetation 1 veg-types))

(define (check-veg vegetation n vlist)
  (cond ((null? vlist) 0)
        ((equal? (nth-item 2 vegetation) (nth-item 2 (car vlist))) n)
        (#t (check-veg vegetation (+ n 1) (cdr vlist)))))

;-------------------------------------------------------------------------------
; Eat: A simple decision tree.
;-------------------------------------------------------------------------------
(define (eat energy prev percepts)
  (if (equal? (nth-item 3 (best-veg veg-types))
              (nth-item 3 (if (list? (get-location percepts 0 1))
                              (get-location percepts 0 1)
                              '())))
    "EAT-AGGRESSIVE"
    (if (and (not (list? (get-location percepts -1 1)))
             (not (list? (get-location percepts 1 1)))
             (not (list? (get-location percepts 0 2))))
      "STAY"
      (cond ((weaker-agent? (if (list? (get-location percepts -1 1))
                                (get-location percepts -1 1)
                                '()) energy)
                (attack (x-loc -1 1) (y-loc -1 1) 'E energy prev percepts))
            ((weaker-agent? (if (list? (get-location percepts 1 1))
                                (get-location percepts 1 1)
                                '()) energy)
                (attack (x-loc 1 1) (y-loc 1 1) 'W  energy prev percepts))
            ((weaker-agent? (if (list? (get-location percepts 0 2))
                                (get-location percepts 0 2)
                               '()) energy)
                (attack (x-loc 0 2) (y-loc 0 2) 'S energy prev percepts))
            (#t "EAT-PASSIVE")))))

;-------------------------------------------------------------------------------
; Get the best vegetation.
;-------------------------------------------------------------------------------
(define (best-veg vlist)
   (cond  ((null? (cdr vlist))
                (car vlist))
          ((> (nth-item 3 (car vlist))
              (nth-item 3 (best-veg (cdr vlist))))
                (car vlist))
          (else (best-veg (cdr vlist)))))

;-------------------------------------------------------------------------------
; Compare agent's energies.
;-------------------------------------------------------------------------------
(define (weaker-agent? agent energy)
   (if (not (equal? (nth-item 1 agent) 'agent))
            #f
            (if (> (/ (+ 1 (- (max-energy agent) energy))
                      (+ 1 (- max-energy min-energy)))
                   0.84)
                #t
                #f)))

(define (max-energy agent)
   (expt 2 (+ (nth-item 3 agent) 1)))

(define (min-energy agent)
   (expt 2 (- (nth-item 3 agent) 1)))

;-------------------------------------------------------------------------------
; Function to guard the vegetation.
;-------------------------------------------------------------------------------
(define (attack x y D energy prev percepts)
  (begin (set! mood "aggressive")
         (set! target (cons '(x y D) target))
         (set! desire "target")
         (seek-target)))

;-------------------------------------------------------------------------------
; Choose action to take.
;-------------------------------------------------------------------------------
(define (explore percepts)
   (cond ((predator-front percepts) (face-L))
         ((barrier-front percepts) (face-L))
         (#t (let ((rand (random 12)))
         (cond ((equal? rand 0) (face-R))
               ((equal? rand 1) (face-L))
               ((equal? rand 2) (face-A))
               ((> rand 9) "MOVE-PASSIVE-3")
               ((> rand 6) "MOVE-PASSIVE-2")
               ((> rand 3) "MOVE-PASSIVE-1"))))))

;-------------------------------------------------------------------------------
;Return true is there is a predator infront.
;-------------------------------------------------------------------------------
(define (predator-front percepts)
   (if (equal? (nth-item 1 (object-front 1 percepts)) 'predator) #t #f))

;-------------------------------------------------------------------------------
; Return true if there is a barrier.
;-------------------------------------------------------------------------------
(define (barrier-front percepts)
   (if (equal? (nth-item 1 (object-front 1 percepts)) 'barrier) #t #f))

;-------------------------------------------------------------------------------
; Check objects right infront.
;-------------------------------------------------------------------------------
(define (object-front n percepts)
   (if (> n 5)
       '(empty)
       (cond ((list? (get-location percepts 0 n))
                  (get-location percepts 0 n))
             ((equal? (get-location percepts 0 n) 'barrier)
                  (list (get-location percepts 0 n)))
             (#t (object-front (+ 1 n) percepts)))))

(define (seek-target)
   (cond ((not (equal? (nth-item 1 location) (nth-item 1 (car target))))
                (move-x))
         ((not (equal? (nth-item 2 location) (nth-item 2 (car target))))
                (move-y))
         ((not (equal? (nth-item 3 location) (nth-item 3 (car target))))
                (move-D))))

;-------------------------------------------------------------------------------
;Face the right direction using decision trees.
;-------------------------------------------------------------------------------
(define (move-x)
   (cond ((> (nth-item 1 location) (nth-item 1 (car target)))
             (if (not (equal? (nth-item 3 location) 'W))
                 (cond ((equal? (nth-item 3 location) 'N) (face-L))
                       ((equal? (nth-item 3 location) 'E) (face-A))
                       ((equal? (nth-item 3 location) 'S) (face-R)))
                 (forward-x)))
         ((< (nth-item 1 location) (nth-item 1 (car target)))
             (if (not (equal? (nth-item 3 location) 'E))
                 (cond ((equal? (nth-item 3 location) 'S) (face-L))
                       ((equal? (nth-item 3 location) 'W) (face-A))
                       ((equal? (nth-item 3 location) 'N) (face-R)))
                 (forward-x)))))

(define (forward-x)
   (cond ((equal? 1 (delta-x))
                (if (equal? mood "peaceful")
                    "MOVE-PASSIVE-1"
                    "MOVE-AGGRESSIVE-1"))
         ((equal? 2 (delta-x))
                (if (equal? mood "peaceful")
                    "MOVE-PASSIVE-2"
                    "MOVE-AGGRESSIVE-2"))
          (#t
                (if (equal? mood "peaceful")
                    "MOVE-PASSIVE-3"
                    "MOVE-AGGRESSIVE-3"))))

(define (move-y)
   (cond ((> (nth-item 2 location) (nth-item 2 (car target)))
              (if (not (equal? (nth-item 3 location) 'S))
                  (cond ((equal? (nth-item 3 location) 'W) (face-L))
                        ((equal? (nth-item 3 location) 'N) (face-A))
                        ((equal? (nth-item 3 location) 'E) (face-R)))
                  (forward-y)))
         ((< (nth-item 2 location) (nth-item 2 (car target)))
              (if (not (equal? (nth-item 3 location) 'N))
                  (cond ((equal? (nth-item 3 location) 'E) (face-L))
                        ((equal? (nth-item 3 location) 'S) (face-A))
                        ((equal? (nth-item 3 location) 'W) (face-R)))
                  (forward-y)))))

(define (forward-y)
   (cond ((equal? 1 (delta-y))
             (if (equal? mood "peaceful")
                 "MOVE-PASSIVE-1"
                 "MOVE-AGGRESSIVE-1"))
         ((equal? 2 (delta-y))
             (if (equal? mood "peaceful")
                 "MOVE-PASSIVE-2"
                 "MOVE-AGGRESSIVE-2"))
         (#t
             (if (equal? mood "peaceful")
                 "MOVE-PASSIVE-3"
                 "MOVE-AGGRESSIVE-3"))))

(define (move-D)
   (cond ((equal? (nth-item 3 (car target)) 'N)
      (cond ((equal? (nth-item 3 location) 'E) (face-L))
            ((equal? (nth-item 3 location) 'S) (face-A))
            ((equal? (nth-item 3 location) 'W) (face-R))))
            ((equal? (nth-item 3 (car target)) 'E)
               (cond ((equal? (nth-item 3 location) 'S)
                         (face-L))
                     ((equal? (nth-item 3 location) 'W)
                         (face-A))
                     ((equal? (nth-item 3 location) 'N)
                         (face-R))))
                     ((equal? (nth-item 3 (car target)) 'S)
                         (cond ((equal? (nth-item 3 location) 'W)
                                   (face-L))
                               ((equal? (nth-item 3 location) 'N)
                                   (face-A))
                               ((equal? (nth-item 3 location) 'E)
                                   (face-R))))
                               ((equal? (nth-item 3 (car target)) 'W)
                                   (cond ((equal? (nth-item 3 location) 'N)
                                              (face-L))
                                         ((equal? (nth-item 3 location) 'E)
                                              (face-A))
                                         ((equal? (nth-item 3 location) 'S)
                                              (face-R))))))

(define (face-L)
   (begin (cond ((equal? (nth-item 3 location) 'N)
                     (set! location (replace-nth-item 3 location 'W)))
                ((equal? (nth-item 3 location) 'E)
                     (set! location (replace-nth-item 3 location 'N)))
                ((equal? (nth-item 3 location) 'S)
                     (set! location (replace-nth-item 3 location 'E)))
                ((equal? (nth-item 3 location) 'W)
                     (set! location (replace-nth-item 3 location 'S))))
          "TURN-LEFT"))

(define (face-R)
   (begin (cond ((equal? (nth-item 3 location) 'N)
                     (set! location (replace-nth-item 3 location 'E)))
                ((equal? (nth-item 3 location) 'E)
                     (set! location (replace-nth-item 3 location 'S)))
                ((equal? (nth-item 3 location) 'S)
                     (set! location (replace-nth-item 3 location 'W)))
                ((equal? (nth-item 3 location) 'W)
                     (set! location (replace-nth-item 3 location 'N))))
          "TURN-RIGHT"))

(define (face-A)
   (begin (cond ((equal? (nth-item 3 location) 'N)
                    (set! location (replace-nth-item 3 location 'S)))
                ((equal? (nth-item 3 location) 'E)
                    (set! location (replace-nth-item 3 location 'W)))
                ((equal? (nth-item 3 location) 'S)
                    (set! location (replace-nth-item 3 location 'N)))
                ((equal? (nth-item 3 location) 'W)
                    (set! location (replace-nth-item 3 location 'E))))
          "TURN-AROUND"))

(define (delta-x)
   (abs (- (nth-item 1 location) (nth-item 1 (car target)))))

(define (delta-y)
   (abs (- (nth-item 2 location) (nth-item 2 (car target)))))

;-------------------------------------------------------------------------------
; Choose action.
;-------------------------------------------------------------------------------
(define (choose energy prev percepts)
   (begin (set! turn-ctr (+ 1 turn-ctr))
          (update-loc prev)
          (check-target)
          (set! desire-ctr (- desire-ctr 1))
          (calm-down prev)
          (cond ((and (moved-0 prev) (not (empty-front percepts)))
                    (evasive-move))
                ((not (null? forced-move))
                    (car forced-move))
                ((and (> desire-ctr 0) food-front percepts)
                    (set-desire "eat"))
                ((and (null? target) (> 1 desire-ctr))
                    (if (not (equal? desire "explore"))
                        (set-desire "explore")
                        (set-target))))))

;-------------------------------------------------------------------------------
; Update location based on spaces moved.
;-------------------------------------------------------------------------------
(define (update-loc prev)
   (if (not (null? (find-history 'moved prev)))
       (let ((delta (nth-item 2 (find-history 'moved prev))))
            (cond ((equal? (nth-item 3 location) 'N)
                        (replace-nth-item 2 location (+ (nth-item 2 location)
                                                        delta)))
                  ((equal? (nth-item 3 location) 'E)
                        (replace-nth-item 1 location (+ (nth-item 1 location)
                                                        delta)))
                  ((equal? (nth-item 3 location) 'S)
                        (replace-nth-item 2 location (- (nth-item 2 location)
                                                        delta)))
                  ((equal? (nth-item 3 location) 'W)
                        (replace-nth-item 1 location (- (nth-item 1 location)
                                                        delta)))))))

;-------------------------------------------------------------------------------
;Look for required events in list of previous events.
;-------------------------------------------------------------------------------
(define (find-history srch plist)
   (cond ((null? plist) (list))
         ((equal? (nth-item 1 (car plist)) srch) (car plist))
         (#t (find-history srch (cdr plist)))))

;-------------------------------------------------------------------------------
; After an attack, change mood to peaceful.
;-------------------------------------------------------------------------------
(define (calm-down prev)
   (if (and (equal? mood "aggressive")
            (not (null? (find-history 'fought prev))))
       (if (moved-0 prev)
           (begin (set! desire-ctr 0)
                  (set! target (list))
                  (set! mood "peaceful"))
           (set! mood "peaceful"))))

(define (check-target)
   (if (null? target)
       (set! target (list))
       (if (equal? location (car target))
           (set! target (list)))))

(define (moved-0 prev)
   (if (equal? (nth-item 2 (find-history 'moved prev)) 0)
       #t
       #f))

(define (empty-front percepts)
   (if (equal? (get-location percepts 0 1) 'empty)
       #t
       #f))

(define (food-front percepts)
   (if (list? (get-location percepts 0 1))
       (if (equal? (get-location percepts 0 1) 'vegetation) #t #f)
        #f))

(define (evasive-move)
   (begin (set! forced-move (cons "MOVE-PASSIVE-1" forced-move))
          (set! forced-move (cons "TURN-RIGHT" forced-move))
          (set! forced-move (cons "MOVE-PASSIVE-1" forced-move))
          (set! forced-move (cons "TURN-LEFT" forced-move))))

(define (set-desire change)
   (begin (if (not (equal? desire change))
          (set! desire-ctr 25))
          (set! desire change)))

(define (set-target)
   (let ((loc (get-plant-loc (best-veg veg-types))))
        (cond ((> (nth-item 1 loc) (nth-item 1 location))
                    (begin (set! target (list))
                           (set! target (cons (list (- (nth-item 1 loc) 1)
                                                    (nth-item 2 loc)
                                              'E)
                                        target))
                           (set-desire "target")))
              ((> (nth-item 1 location) (nth-item 1 loc))
                    (begin (set! target (list))
                           (set! target (cons (list (+ (nth-item 1 loc) 1)
                                                       (nth-item 2 loc)
                                                    'W)
                                              target))
                           (set-desire "target")))
              ((> (nth-item 2 location) (nth-item 2 loc))
                    (begin (set! target (list))
                           (set! target (cons (list (nth-item 1 loc)
                                                    (+ (nth-item 2 loc) 1)
                                              'S)
                                        target))
                           (set-desire "target")))
              ((> (nth-item 2 loc) (nth-item 2 location))
                    (begin (set! target (list))
                           (set! target (cons (list (nth-item 1 loc)
                                                    (- (nth-item 2 loc) 1)
                                              'N)
                                        target))
                           (set! desire "target")))
              (#t location))))

(define (get-plant-loc veg)
   (if (search-chart veg chart (list))
       (list (nth-item 2 (nearest (search-chart veg chart (list))))
             (nth-item 3 (nearest (search-chart veg chart (list)))))
       location))

(define (search-chart veg clist answer)
   (if (and (null? clist) (null? answer))
       #f
       (cond ((and (null? clist) (not (null? answer)))
                      answer)
             ((equal? (nth-item 2 veg) (nth-item 2 (car (car clist))))
                      (search-chart veg (cdr clist) (cons (car clist) answer)))
             (#t (search-chart veg (cdr clist) answer)))))

(define (nearest vlist)
   (cond ((null? (cdr vlist))
                (car vlist))
         ((< (distance (car vlist)) (distance (nearest (cdr vlist))))
                (car vlist))
         (else (nearest (cdr vlist)))))

(define (distance vegatable)
   (+ ;sum
   (+ (abs (nth-item 1 location)) (abs (nth-item 2 vegatable)))
   (+ (abs (nth-item 2 location)) (abs (nth-item 3 vegatable)))))

(define (nth-item idx alist)
   (cond ((null? alist) alist)
         ((= idx 1) (car alist))
         (#t (nth-item (- idx 1) (cdr alist)))))

(define (replace-nth-item idx alist n)
   (cond ((= idx 1) (append (list n) (cdr alist)))
         (#t (append (list (car alist))
                     (replace-nth-item (- idx 1) (cdr alist) n)))))

(define (get-location percept x y)
   (nth-item (+ x (+ y 1)) (nth-item y percept)))
