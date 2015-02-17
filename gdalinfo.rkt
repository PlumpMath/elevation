#lang racket

(provide parse-gdal-info)

(define (parse-gdal-info info-str)
  (let ([driver (second (regexp-match #px"Driver: (\\S+)" info-str))]
        [files  (second (regexp-match #px"Files: (\\S+)" info-str))]
        [width  (second (regexp-match #px"Size is (\\d+), \\d+\n" info-str))]
        [height (second (regexp-match #px"Size is \\d+, (\\d+)\n" info-str))]
        [geogcs (second (regexp-match #px"GEOGCS\\[\"(.*?)\"," info-str))]
        [datum  (second (regexp-match #px"DATUM\\[\"(.*?)\"," info-str))]
        [spheroid-name (second (regexp-match #px"SPHEROID\\[\"(.*?)\"," info-str))]
        [spheroid-semi-major-axis (second (regexp-match #px"SPHEROID\\[\".*?\",(\\d+\\.*\\d*)" info-str))]
        [spheroid-inverse-flattening (second (regexp-match #px"SPHEROID\\[\".*?\",\\d+\\.*\\d*,(\\d+\\.*\\d*)," info-str))]
        [spheroid-authority-name
          (second (regexp-match #px"SPHEROID\\[\".*?\",\\d+\\.*\\d*,\\d+\\.*\\d*,\\s*AUTHORITY\\[\"(\\S+)\","
                                info-str))]
        [spheroid-authority-id
          (second (regexp-match #px"SPHEROID\\[\".*?\",\\d+\\.*\\d*,\\d+\\.*\\d*,\\s*AUTHORITY\\[\"\\S+\",\"(\\d+)\""
                                info-str))]
        [primem-name (second (regexp-match #px"PRIMEM\\[\"(\\S+)\"" info-str))]
        [primem-value (second (regexp-match #px"PRIMEM\\[\"\\S+\",(\\d+)" info-str))]
        [unit-name (second (regexp-match #px"UNIT\\[\"(\\S+)\"" info-str))]
        [unit-value (second (regexp-match #px"UNIT\\[\"\\S+\",(\\d+\\.\\d+)" info-str))])
    (begin
      (displayln driver)
      (displayln files)
      (displayln width)
      (displayln height)
      (displayln geogcs)
      (displayln datum)
      (displayln spheroid-name)
      (displayln spheroid-semi-major-axis)
      (displayln spheroid-inverse-flattening)
      (displayln spheroid-authority-name)
      (displayln spheroid-authority-id)
      (displayln primem-name)
      (displayln primem-value)
      (displayln unit-name)
      (displayln unit-value))))


