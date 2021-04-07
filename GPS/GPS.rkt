#lang racket

(require racket/set)

(define every andmap)
(define some ormap)

(struct op (action preconds add-list del-list))

(define *state* '(son-at-home have-money car-needs-battery have-phone-book))
(define *ops* (list (op 'drive-son-to-school '(son-at-home car-works) '(son-at-school) '(son-at-home))
                    (op 'shop-installs-battery '(car-needs-battery shop-knows-problem shop-has-money) '(car-works) '())
                    (op 'tell-shop-problem '(in-communication-with-shop) '(shop-knows-problem) '())
                    (op 'telephone-shop '(know-phone-number) '(in-communication-with-shop) '())
                    (op 'look-up-number '(have-phone-book) '(know-phone-number) '())
                    (op 'give-shop-money '(have-money) '(shop-has-money) '(have-money))))

(define (GPS goals)
  (if (every achieve goals) 'solved 'failed))

(define (achieve goal)
  (or (member goal *state*)
      (some apply-op
           (filter (λ (op) (appropriate-op goal op)) *ops*))))

(define (appropriate-op goal op)
  (member goal (op-add-list op)))

(define (apply-op op)
  (if (not (void? (when (every achieve (op-preconds op))
                    (displayln (list 'executing (op-action op)))
                    (set! *state* (set-subtract *state* (op-del-list op)))
                    (set! *state* (set-union *state* (op-add-list op)))
                    #t)))
      #t #f))

(GPS '(son-at-school))
