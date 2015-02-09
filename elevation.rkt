#lang racket

(provide elevation-rasters)

(require pict)
(require json)
(require net/url)
(require file/unzip)

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

;; (: make-geotiff-copy (-> SRTM Void))
(define (make-geotiff-copy srtm)
  (λ (dir)
     (let ([geotiff (string-append (path->string dir)
                                   "/"
                                   (SRTM-file-name srtm)
                                   ".tif ")])
       (system (string-append "cp -f "
                              geotiff
                              "data/")))))

;; (: SRTM-download (-> SRTM Void))
(define (SRTM-download srtm)
  (let* ([url-prefix "http://gis-lab.info/data/srtm-tif/"]
         [url-path (string-append url-prefix (SRTM-file-name srtm) ".zip")]
         [url (string->url url-path)]
         [geotiff-copy (make-geotiff-copy srtm)])
    (call-with-unzip (get-pure-port url #:redirections 1)
                     geotiff-copy)))

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

;; (: SRTM-intersection (-> SRTM Real Real Real Real Bitmap))
(define (SRTM-intersection srtm min-long min-lat max-long max-lat)
  (let ([target-resolution (/ 0.000833333333333 90)]
        [interpolation-method "cubicspline"]
        [scale-min 0]
        [scale-max 2200]
        [out-size 129])
    (begin
      (SRTM-download srtm)
      (system (string-append "rm -f "
                             "data/"
                             (SRTM-file-name srtm)
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
                             "data/"
                             (SRTM-file-name srtm)
                             ".tif "
                             "data/"
                             (SRTM-file-name srtm)
                             "_cropped.tif"))
      (system (string-append "gdal_translate -q -ot Byte -of BMP "
                             "-scale "
                             (number->string scale-min)
                             " "
                             (number->string scale-max)
                             " "
                             "-outsize "
                             (number->string out-size)
                             " "
                             (number->string out-size)
                             " "
                             "data/"
                             (SRTM-file-name srtm)
                             "_cropped.tif "
                             "data/"
                             (SRTM-file-name srtm)
                             "_cropped.bmp"))
      (bitmap (string-append (SRTM-file-name srtm)
                             "_cropped.bmp")))))

;; (: elevation-rasters (-> Real Real Real Real (Listof Bitmap)))
(define (elevation-rasters min-long min-lat max-long max-lat)
  (map (λ (srtm)
          (SRTM-intersection srtm min-long min-lat max-long max-lat))
       (filter (SRTM-intersects? min-long min-lat max-long max-lat)
               SRTMs)))
