#!/usr/bin/env racket

#lang racket

(require "elevation.rkt")

;; (: request-params (HashTable String String))
(define request-params
  (with-handlers
    ([exn:fail:contract? (λ (_) (hash))])
    (apply hash
           (flatten (map (λ (str)
                            (string-split str "="))
                         (string-split (getenv "QUERY_STRING") "&"))))))

(define request-format 'n-triples)
(define request-proj 'utm)
(define request-scale 90)

(define request-bbox
  (vector->list (string->bbox (hash-ref request-params "bbox" "0.0,0.0,0.0,0.0"))))

(define (print-body)
  (with-input-from-file
      (SRTM->n-triples-file
        (apply SRTM-inside request-bbox))
      (λ () (copy-port (current-input-port)
                       (current-output-port)))))

(module+ main
  (printf "Content-Type: text/plain~n~n")
  (print-body))
