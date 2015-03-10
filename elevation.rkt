#lang racket

;; Prereqs:
;; * Python 2.7.5
;; * GDAL

(provide elevation-raster
         elevation-service-start)

(require pict)
(require json)
(require net/url)
(require file/unzip)
(require file/convertible)
(require web-server/servlet)
(require (planet dmac/spin))

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

;; (: SRTM-intersection (-> (Listof SRTM) Real Real Real Real String))
(define (SRTM-intersection srtms min-long min-lat max-long max-lat)
  (let ([file-name-prefix (foldl (λ (srtm str)
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
      (for/list ([srtm srtms])
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
       (printf "Translating GeoTiff file to PNG.\n")
       (time (system (string-append "gdal_translate "
                                   "-q "
                                   "-ot Byte -of PNG "
                                   data-dir
                                   file-name-prefix
                                   "cropped.tif "
                                   data-dir
                                   file-name-prefix
                                   "cropped.png")))
      (printf "Translating GeoTiff file to XYZ.\n")
      (time (system (string-append "gdal_translate "
                                   ""
                                   "-of XYZ "
                                   data-dir
                                   file-name-prefix
                                   "cropped.tif "
                                   data-dir
                                   file-name-prefix
                                   "cropped.xyz")))
      (string-append data-dir
                     file-name-prefix
                     "cropped"))))

;; (: elevation-raster (-> Real Real Real Real String))
(define (elevation-raster min-long min-lat max-long max-lat)
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
                 (string-append (elevation-raster (string->number (params req 'minlong))
                                                  (string->number (params req 'minlat))
                                                  (string->number (params req 'maxlong))
                                                  (string->number (params req 'maxlat)))
                                ".png"))
              #"image/png")
    (file-get "/xyz"
              (λ (req)
                 (string-append (elevation-raster (string->number (params req 'minlong))
                                                  (string->number (params req 'minlat))
                                                  (string->number (params req 'maxlong))
                                                  (string->number (params req 'maxlat)))
                                ".xyz"))
              #"text/plain")
    (run)))

;; Entry point:
;; $ racket elevation.rkt
(module+ main
  (elevation-service-start))

