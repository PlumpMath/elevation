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

;; (: SRTM-intersection (-> (Listof SRTM) Real Real Real Real Bitmap))
(define (SRTM-intersection srtms min-long min-lat max-long max-lat)
  (let ([file-name-prefix (foldl (λ (srtm str)
                                    (string-append (SRTM-file-name srtm)
                                                   "_"
                                                   str))
                                 ""
                                 srtms)]
        [target-resolution (/ 0.0008333333333 90)] ;; 1 metre resolution
        [interpolation-method "cubicspline"]
        [scale-min 0]
        [scale-max 256])
    (begin
      (map SRTM-download srtms)
      (system (string-append "gdal_merge.py "
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
                                    srtms)))
      (system (string-append "rm -f "
                             data-dir
                             file-name-prefix
                             "_cropped.tif"))
      (system (string-append "gdalwarp "
                             "-q "
                             "-r "
                             interpolation-method
                             " "
                             "-tr "
                             (number->string target-resolution)
                             " "
                             (number->string (* -1 target-resolution))
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
                             "_cropped.tif"))
      (system (string-append "gdal_translate "
                             "-q "
                             "-ot Byte -of BMP "
                             "-scale "
                             (number->string scale-min)
                             " "
                             (number->string scale-max)
                             " "
                             data-dir
                             file-name-prefix
                             "_cropped.tif "
                             data-dir
                             file-name-prefix
                             "_cropped.bmp"))
      (bitmap (string-append data-dir
                             file-name-prefix
                             "_cropped.bmp")))))

;; (: elevation-raster (-> Real Real Real Real Bitmap))
(define (elevation-raster min-long min-lat max-long max-lat)
  (let ([srtms (filter (SRTM-intersects? min-long min-lat max-long max-lat) SRTMs)])
    (SRTM-intersection srtms min-long min-lat max-long max-lat)))

;; (: elevation-service-start (-> Void))
(define (elevation-service-start)
  (define (bitmap-response-maker status headers body)
    (response status
              (status->message status)
              (current-seconds)
              #"image/png"
              headers
              (λ (op)
                 (write-bytes (convert body 'png-bytes) op))))
  (define (bitmap-get path handler)
    (define-handler "GET" path handler bitmap-response-maker))
  (begin
    (bitmap-get "/"
                (λ (req)
                   (elevation-raster (string->number (params req 'minlong))
                                             (string->number (params req 'minlat))
                                             (string->number (params req 'maxlong))
                                             (string->number (params req 'maxlat)))))
    (run)))
