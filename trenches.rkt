#lang racket

(provide muninn/trenches)

(require json)
(require net/url)
(require net/uri-codec)

;; (: sparql-url String)
(define sparql-url "http://rdf.muninn-project.org/sparql")

;; (: sparql-default-graph-uri String)
(define sparql-default-graph-uri "http://rdf.muninn-project.org/")

;; (: sparql-format String)
(define sparql-format "json")

;; (: sparql-timeout Number)
(define sparql-timeout 30000)

;; (: sparql-debug String)
(define sparql-debug "on")

;; (: make-sparql-query-url (-> String String))
(define (make-sparql-query-url query)
  (string-append sparql-url
                 "?"
                 "default-graph-uri="
                 (uri-encode sparql-default-graph-uri)
                 "&query="
                 (uri-encode query)
                 "&format="
                 (uri-encode sparql-format)
                 "&timeout="
                 (number->string sparql-timeout)
                 "&debug="
                 sparql-debug))

;; (: get/json (-> String JSExpr)
(define (get/json str)
  (call/input-url (string->url str)
                  get-pure-port
                  read-json))

;; (: make-objects-query (-> Real Real Real Real String)
(define (make-objects-query min-long min-lat max-long max-lat)
  (string-append "SELECT DISTINCT ?thing ?state "
    "{ "
    "  ?thing <http://geovocab.org/geometry#geometry> ?geoma . "
    "  ?thing <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>  <http://rdf.muninn-project.org/ontologies/military#MilitaryTrench> . "
    "  ?geoma <http://linkedgeodata.org/ontology/posSeq> ?SEQ . "
    "  ?SEQ <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/1999/02/22-rdf-syntax-ns#Seq> . "
    "  ?SEQ ?List ?node . "
    "  ?node <http://www.w3.org/2003/01/geo/wgs84_pos#lat> ?LAT . "
    "  ?node <http://www.w3.org/2003/01/geo/wgs84_pos#long> ?LONG . "
    " OPTIONAL {?thing <http://rdf.muninn-project.org/ontologies/graves#hasState> ?state . } "
    " FILTER (?LAT < "
    (number->string max-lat)
    " && ?LAT > "
    (number->string min-lat)
    " ) "
    " FILTER (?LONG < "
    (number->string max-long)
    "  && ?LONG > "
    (number->string min-long)
    " ) "
    "}"))

;; (: muninn/things (-> Real Real Real Real (Listof String)))
(define (muninn/things min-long min-lat max-long max-lat)
  (let* ([response (get/json (make-sparql-query-url (make-objects-query min-long min-lat max-long max-lat)))]
         [bindings (hash-ref (hash-ref response 'results) 'bindings)])
    (map (λ (binding)
            (hash-ref (hash-ref binding 'thing) 'value))
         bindings)))

;; (: make-trenches-query (-> String Real Real Real Real String))
(define (make-trenches-query trench min-long min-lat max-long max-lat)
  (string-append "SELECT ?List  ?LAT  ?LONG "
    "{ "
    "  <" trench "> <http://geovocab.org/geometry#geometry> ?geoma . "
    "  <" trench "> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>  <http://rdf.muninn-project.org/ontologies/military#MilitaryTrench> . "
    "  ?geoma <http://linkedgeodata.org/ontology/posSeq> ?SEQ . "
    "  ?SEQ <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/1999/02/22-rdf-syntax-ns#Seq> . "
    "  ?SEQ ?List ?node . "
    "  ?node <http://www.w3.org/2003/01/geo/wgs84_pos#lat> ?LAT . "
    "  ?node <http://www.w3.org/2003/01/geo/wgs84_pos#long> ?LONG . "
    "  FILTER (?LAT < "
    (number->string max-lat)
    " || ?LAT > "
    (number->string min-lat)
    " || ?LONG < "
    (number->string max-long)
    " || ?LONG > "
    (number->string min-long)
    " ) "
    "} "
    "ORDER BY ASC(?List)"))

;; (: muninn/trench (-> Real Real Real Real String (Listof Number)))
(define (muninn/trench min-long min-lat max-long max-lat)
  (λ (trench)
     (let* ([response (get/json (make-sparql-query-url (make-trenches-query trench min-long min-lat max-long max-lat)))]
            [bindings (hash-ref (hash-ref response 'results) 'bindings)])
     (map (λ (binding)
             (list (string->number (hash-ref (hash-ref binding 'LONG) 'value))
                   (string->number (hash-ref (hash-ref binding 'LAT) 'value))))
          bindings))))

;; (: muninn/trenches (-> String Real Real Real Real (Listof (Listof Number))))
(define (muninn/trenches min-long min-lat max-long max-lat)
  (let ([things (muninn/things min-long min-lat max-long max-lat)])
    (map (muninn/trench min-long min-lat max-long max-lat)
         things)))

