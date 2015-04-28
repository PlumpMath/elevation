#!/usr/bin/env racket

#lang racket

(require "elevation.rkt")

;; (: log-file String)
(define log-file "log.txt")

;; (: request-params (HashTable String String))
(define request-params
  (with-handlers
    ([exn:fail:contract? (λ (_) (hash))])
    (apply hash
           (flatten (map (λ (str)
                            (string-split str "="))
                         (string-split (getenv "QUERY_STRING") "&"))))))

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

(define (mime-type request-format)
  (cond [(equal? request-format "rdf/xml")
         "application/rdf+xml"]
        [(equal? request-format "n3")
         "text/rdf+n3"]
        [(equal? request-format "ntriples")
         "text/plain"]
        [(equal? request-format "xyz")
         "text/plain"]
        [(equal? request-format "png")
         "image/png"]))

(define (print-body request-file)
  (with-input-from-file
      request-file
      (λ () (copy-port (current-input-port)
                       (current-output-port)))))

(define (request-file request-format request-bbox)
  (let ([srtm-inside (apply SRTM-inside (vector->list request-bbox))])
    (cond [(equal? request-format "rdf/xml")
           (SRTM->rdf-xml-file srtm-inside)]
          [(equal? request-format "n3")
           (SRTM->n3-file srtm-inside)]
          [(equal? request-format "ntriples")
           (SRTM->n-triples-file srtm-inside)]
          [(equal? request-format "xyz")
           (SRTM->xyz-file srtm-inside)]
          [(equal? request-format "png")
           (SRTM->png-file srtm-inside)])))

(module+ main
  (with-handlers
    [(exn:fail:contract?
       (λ (exn)
          (printf "Status: 406 Not Acceptable~n~n~a~n" exn)))]
    (let* ([request-bbox (string->bbox (hash-ref request-params "bbox"))]
           [request-format (hash-ref request-params "format")]
           [request-file
             (with-output-to-file
               log-file #:exists 'append
               (λ () (request-file request-format request-bbox)))])
      (begin
        (check-bbox request-bbox)
        (check-format request-format)
        (printf "Status: 200 OK~n")
        (printf "Content-Type: ~a~n~n" (mime-type request-format))
        (print-body request-file)
        (with-output-to-file
          log-file #:exists 'append
          (λ ()
             (begin
               (time (system (string-append "rm -f " request-file)))
               (printf "Removed \"~a\".~n" request-file)
               (printf "Done request.~n"))))))))
