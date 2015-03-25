#lang racket

;; Prereqs:
;; * Python 2.7.5
;; * GDAL

(provide SRTM-inside
         elevation-service-start)

(require pict)
(require json)
(require net/url)
(require file/unzip)
(require file/convertible)
(require web-server/servlet)
(require (planet dmac/spin))
(require data/utm)

;; (: data-dir String)
(define data-dir
  (let ([path "/tmp/data/"])
    (begin
      (system (string-append  "mkdir -p " path))
      path)))

;; (: SRTM String Real Real Real Real)
(struct SRTM (file-name min-long min-lat max-long max-lat) #:transparent)

;; (: SRTMs (Listof SRTM))
(define SRTMs
  (let ([features (hash-ref (call-with-input-file "world.json" read-json) 'features)])
    (map (λ (feature)
            (let* ([coordinates (first (hash-ref (hash-ref feature 'geometry) 'coordinates))]
                   [x-values (map first coordinates)]
                   [y-values (map second coordinates)]
                   [min-long (apply min x-values)]
                   [max-long (apply max x-values)]
                   [min-lat (apply min y-values)]
                   [max-lat (apply max y-values)])
              (SRTM (hash-ref (hash-ref feature 'properties) 'filename)
                    min-long min-lat max-long max-lat)))
         (filter (λ (feature)
                    (hash-has-key? (hash-ref feature 'properties) 'filename))
                 features))))

;; (: SRTM-intersects? (-> Real Real Real Real (-> SRTM Boolean)))
(define (SRTM-intersects? min-long min-lat max-long max-lat)
  (λ (srtm)
     (and (not (< (SRTM-max-long srtm)
                  min-long))
          (not (> (SRTM-min-long srtm)
                  max-long))
          (not (< (SRTM-max-lat srtm)
                  min-lat))
          (not (> (SRTM-min-lat srtm)
                  max-lat)))))

;; (: SRTM-download (-> SRTM Void))
(define (SRTM-download srtm)
  ;; (: make-geotiff-copy (-> SRTM Void))
  (define (make-geotiff-copy srtm)
    (λ (dir)
       (let ([geotiff (string-append (path->string dir)
                                     "/"
                                     (SRTM-file-name srtm)
                                     ".tif ")])
         (system (string-append "cp -f "
                                geotiff
                                data-dir)))))
  (let* ([url-prefix "http://gis-lab.info/data/srtm-tif/"]
         [url-path (string-append url-prefix (SRTM-file-name srtm) ".zip")]
         [url (string->url url-path)]
         [geotiff-copy (make-geotiff-copy srtm)])
    (call-with-unzip (get-pure-port url #:redirections 1)
                     geotiff-copy)))

;; (: SRTM->png-file (-> SRTM String))
(define (SRTM->png-file srtm)
  (let ([file-name-prefix (list->string (drop-right (string->list (SRTM-file-name srtm)) 4))])
    (begin
      (printf "Translating GeoTiff file to PNG.\n")
      (time (system (string-append "gdal_translate "
                                   "-q "
                                   "-ot Byte -of PNG "
                                   file-name-prefix
                                   ".tif "
                                   file-name-prefix
                                   ".png")))
      (string-append file-name-prefix ".png"))))

;; (: SRTM->xyz-file (-> SRTM String))
(define (SRTM->xyz-file srtm)
  (let ([file-name-prefix (list->string (drop-right (string->list (SRTM-file-name srtm)) 4))])
    (begin
      (printf "Translating GeoTiff file to XYZ.\n")
      (time (system (string-append "gdal_translate"
                                   " "
                                   "-of XYZ "
                                   file-name-prefix
                                   ".tif "
                                   file-name-prefix
                                   ".xyz")))
      (string-append file-name-prefix ".xyz"))))

;; (: SRTM-utm-project (-> SRTM SRTM))
(define (SRTM-utm-project srtm)
  (let* ([file-name-prefix (list->string (drop-right (string->list (SRTM-file-name srtm)) 4))]
         [new-file-name (string-append file-name-prefix "_utm.tif")]
         [zone (utm-zone (* 0.5
                            (+ (SRTM-min-long srtm)
                               (SRTM-max-long srtm)))
                         (* 0.5
                            (+ (SRTM-min-lat srtm)
                               (SRTM-max-lat srtm))))])
    (begin
      (printf "UTM-projecting GeoTiff file.~n")
      (printf "Calculated UTM zone: ~a~n" zone)
      (when (file-exists? new-file-name)
        (begin
          (printf "Removing old UTM-projected GeoTiff file.~n")
          (time (system (string-append "rm -f " new-file-name)))))
      (time (system (string-append "gdalwarp"
                                   " "
                                   (SRTM-file-name srtm)
                                   " "
                                   new-file-name
                                   " "
                                   "-q"
                                   " "
                                   "-t_srs "
                                   "\""
                                   "+proj=utm"
                                   " "
                                   "+zone="
                                   (number->string zone)
                                   " "
                                   "+ellps=WGS84"
                                   "\"")))
      (SRTM new-file-name
            (SRTM-min-long srtm)
            (SRTM-min-lat  srtm)
            (SRTM-max-long srtm)
            (SRTM-max-lat  srtm)))))

;; (: SRTM-intersection (-> (Listof SRTM) Real Real Real Real SRTM))
(define (SRTM-intersection srtms min-long min-lat max-long max-lat)
  (let* ([file-name-prefix (foldl (λ (srtm str)
                                     (string-append (SRTM-file-name srtm)
                                                    "_"
                                                    str))
                                  ""
                                  srtms)]
         [interpolation-method "bilinear"])
    (begin
      (printf "\n")
      (printf "Downloading ~a tiles of elevation data.\n"
              (length srtms))
      (for ([srtm srtms])
        (begin (printf "Downloading ~s.\n"
                       (SRTM-file-name srtm))
               (time
                 (unless (file-exists? (string-append data-dir
                                                      (SRTM-file-name srtm)
                                                      ".tif"))
                   (SRTM-download srtm)))))
      (printf "Merging tiles together.\n")
      (time (system (string-append "gdal_merge.py "
                                   "-q "
                                   "-of GTiff "
                                   "-o "
                                   data-dir
                                   file-name-prefix
                                   ".tif "
                                   (foldl (λ (srtm str)
                                             (string-append data-dir
                                                            (SRTM-file-name srtm)
                                                            ".tif "
                                                            str))
                                          ""
                                          srtms))))
      (printf "Removing old cropped/merged GeoTiff file.\n")
      (time (system (string-append "rm -f "
                                   data-dir
                                   file-name-prefix
                                   "cropped.tif")))
      (printf "Cropping GeoTiff file.\n")
      (time (system (string-append "gdalwarp "
                                   "-q "
                                   " "
                                   "-r "
                                   interpolation-method
                                   " "
                                   "-te "
                                   (number->string min-long)
                                   " "
                                   (number->string min-lat)
                                   " "
                                   (number->string max-long)
                                   " "
                                   (number->string max-lat)
                                   " "
                                   data-dir
                                   file-name-prefix
                                   ".tif "
                                   data-dir
                                   file-name-prefix
                                   "cropped.tif")))
      (SRTM (string-append data-dir file-name-prefix "cropped.tif")
            min-long min-lat max-long max-lat))))

;; (: SRTM-inside (-> Real Real Real Real SRTM))
(define (SRTM-inside min-long min-lat max-long max-lat)
  (let ([srtms (filter (SRTM-intersects? min-long min-lat max-long max-lat) SRTMs)])
    (SRTM-intersection srtms min-long min-lat max-long max-lat)))

;; (: elevation-service-start (-> Void))
(define (elevation-service-start)
  (define (file-response-maker mime-type)
    (λ (status headers body)
       (response status
                 (status->message status)
                 (current-seconds)
                 mime-type
                 headers
                 (λ (op)
                    (begin
                      (with-input-from-file body
                                            (λ () (copy-port (current-input-port) op)))
                      (void))))))
  (define (file-get path handler mime-type)
    (define-handler "GET" path handler (file-response-maker mime-type)))
  (begin
    (file-get "/"
              (λ (req)
                 (SRTM->png-file
                   (SRTM-inside (string->number (params req 'minlong))
                                (string->number (params req 'minlat))
                                (string->number (params req 'maxlong))
                                (string->number (params req 'maxlat)))))
              #"image/png")
    (file-get "/xyz"
              (λ (req)
                 (SRTM->xyz-file
                   (SRTM-inside (string->number (params req 'minlong))
                                (string->number (params req 'minlat))
                                (string->number (params req 'maxlong))
                                (string->number (params req 'maxlat)))))
              #"text/plain")
    (file-get "/utm"
              (λ (req)
                 (SRTM->png-file
                   (SRTM-utm-project
                     (SRTM-inside (string->number (params req 'minlong))
                                  (string->number (params req 'minlat))
                                  (string->number (params req 'maxlong))
                                  (string->number (params req 'maxlat))))))
              #"image/png")
    (run)))

;; Entry point:
;; $ racket elevation.rkt
(module+ main
  (elevation-service-start))

