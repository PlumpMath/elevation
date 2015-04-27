#lang racket

(provide string->bbox)

;; (: string->bbox (-> String (U (Vector Number Number Number Number) False)))
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
    [#f #f]))
