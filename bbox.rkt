#lang racket

(provide string->bbox)

;; (: string->bbox (-> String (Vector Number Number Number Number)))
(define (string->bbox str)
  (match
    (regexp-match
      #rx"^(\\-?[0-9]+\\.?[0-9]*),[ ]?([0-9]+\\.?[0-9]*),[ ]?([0-9]+\\.?[0-9]*),[ ]?([0-9]+\\.?[0-9]*)$"
      str)
    [(list _ min-long min-lat max-long max-lat)
     (vector (string->number min-long)
             (string->number min-lat)
             (string->number max-long)
             (string->number max-lat))]
    [#f (vector 0.0 0.0 0.0 0.0)]))
