#!/usr/bin/env racket

#lang racket

(require "bbox.rkt")
(require "elevation.rkt")

;; (: request-params (HashTable String String))
(define request-params
  (with-handlers
    ([exn:fail:contract? (λ (_) (hash))])
    (apply hash
           (flatten (map (λ (str)
                            (string-split str "="))
                         (string-split (getenv "QUERY_STRING") "&"))))))

(define (check-bbox request-bbox)
  (cond [(and (vector? request-bbox)
              (equal? (vector-length request-bbox) 4)
              (>  (abs (- (vector-ref request-bbox 2)
                          (vector-ref request-bbox 0)))
                  0)
              (>  (abs (- (vector-ref request-bbox 3)
                          (vector-ref request-bbox 1)))
                  0))
         request-bbox]
        [else
          (raise-argument-error
            'check-bbox
            (string-append
              "(Where \n"
              "  (Vector [min-long : Number]\n"
              "          [min-lat : Number]\n"
              "          [max-long : Number]\n"
              "          [max-lat : Number])\n"
              "  (And (> (abs (- max-long min-long) 0))\n"
              "       (> (abs (- max-lat min-lat) 0))))")
              request-bbox)]))

(define (check-format request-format)
  (cond [(and (not (equal? request-format "rdf/xml"))
              (not (equal? request-format "n3"))
              (not (equal? request-format "ntriples"))
              (not (equal? request-format "xyz"))
              (not (equal? request-format "png")))
         (raise-argument-error
           'check-format
           "(U \"rdf/xml\" \"n3\" \"ntriples\" \"xyz\" \"png\")"
           request-format)]
        [else request-format]))

(define (check-proj request-proj)
  (cond [(and (not (equal? request-proj "utm"))
              (not (equal? request-proj "wgs84")))
         (raise-argument-error
           'check-proj
           "(U \"utm\" \"wgs84\")"
           request-proj)]
        [else request-proj]))

(define (check-scale request-scale)
  (cond [(or (not (number? request-scale))
             (> request-scale 90)
             (< request-scale 1))
         (raise-argument-error
           'check-scale
           "(Range 1 90)"
           request-scale)]
        [else request-scale]))

(define (print-body request-bbox)
  (with-input-from-file
      (SRTM->n-triples-file
        (apply SRTM-inside (vector->list request-bbox)))
      (λ () (copy-port (current-input-port)
                       (current-output-port)))))

(module+ main
  (with-handlers
    [(exn:fail:contract?
       (λ (exn)
          (printf "Status: 406 Not Acceptable~n~n~a~n" exn)))]
    (let ([request-bbox (string->bbox (hash-ref request-params "bbox"))]
          [request-format (hash-ref request-params "format")]
          [request-scale (string->number (hash-ref request-params "scale"))]
          [request-proj (hash-ref request-params "proj")])
      (begin
        (check-bbox request-bbox)
        (check-format request-format)
        (check-scale request-scale)
        (check-proj request-proj)
        (printf "Content-Type: text/plain~n~n")
        (print-body request-bbox)))))
