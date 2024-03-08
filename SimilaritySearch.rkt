#lang racket
#|
##################################################################
Programming Paradigms - CSI 2120
Anthony Le - 300287511
Ali Zaidi - 300080502

March 6th, 2024.

Assignment 2: SimilaritySearch
##################################################################
|#

; Final function that calls for every other method.

#|

(define  (similaritySearch queryHistogramFilename imageDatasetDirectory)

  )


;
(define (takeInFile filname)
  
  )

|#

#|
(define (convert-list-to-vec lst)
  (list->vector (cdr lst))
  )
|#

;################################################################################
;FUNCTION TO LOOP THROUGH THE IMAGE DATASET TO COMPARE VALUES.


; returns a list with all the values; we use this to create the list that has the values themselves.
(define (loop-through-dataset lst query)
  (if (null? lst)
      '()
      (let ((photo (convert-file-path (car lst))))
        (if (ends-with-jpg photo)
            (cons (division (compare-histogram query (convert-file-to-list photo)) (sum-of-img (convert-file-to-list photo))) (loop-through-dataset (cdr lst) query))
            (loop-through-dataset (cdr lst) query)
            )
        )
      )
  )



;################################################################################
;FUNCTION TO CHECK IF THE FILENAME ENDS WITH JPG.TXT
(define (ends-with-jpg photo)
  (if (string=? (substring photo (- (string-length photo)(- (string-length "jpg.txt") 1 ))) ".jpg.txt")
        #t
        #f
      )
  )

(define (isSubString s1 s2)
  (let ((str-len (string-length s2))
        
        (sub-len (string-length s1))
        )
    (let strLoop ((start-idx 0))
      (let ((sub-idx(+ start-idx sub-len)))
        (if (= start-idx (- str-len 1))
            #f
            (if ( string=? (substring s2 start-idx sub-idx) s1)
                #t
                (strLoop (+ start-idx 1))
                )
            )
        )
      )
    )
  )
;################################################################################
;function to convert a directory into a list;
(define (convert-directory dirName)
  (directory-list dirName)
  )

(define (convert-file-path filepath)
  (path->string filepath)
  )
;################################################################################
;function to sort the list; keep in mind we are going to use the
;(sort '() >) to sort it; the > or < would be used to sort it depending on how we want it

(define (final-sort lst)
  (sort lst >)
  )

; FUNCTION TO CONVERT COLORHISTOGRAM TO LIST
;##############################################################################33

(define (convert-file-to-list filename)
  (let ((p (open-input-file filename)))
    (let f ((x (read p)))
      (if (eof-object? x)
          (begin
            (close-input-port p)
            '())
          (cons x (f (read p)))
          )
      )
    )
  )

;;FUNCTION TO DO THE COMPARING (SIMILAR IN THE COMPARE(HIST1, HIST2) METHOD IN JAVA).
; ######################################################################################
 ; compares the two histograms and returns the intersection over the sum through a factorial recursion method.
; this returns the intersection part of the (intersection / sum) algorithm to compare.
(define (compare-histogram lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      0
      (let ((val1 (car lst1)) (val2 (car lst2)))
        (if (>= val1 val2)
         (+ val2 (compare-histogram (cdr lst1) (cdr lst2)))
         (+ val1 (compare-histogram (cdr lst1) (cdr lst2)))
                )
          )
        )
      )

(define (sum-of-img lst) ; gets the sum in the (intersection / sum) algorithm to compare
  (if (null? lst)
      0
      ( + (car lst) (sum-of-img (cdr lst)))
      )
  )

(define (division x y)
  (/ x y)
  )

;TEST CASES
;#########################################################################################3
 (let ((x (compare-histogram (convert-file-to-list "savedQueryHistograms/colorHistogram_4.txt") (convert-file-to-list "savedQueryHistograms/colorHistogram_14.txt")))
        (y (sum-of-img (convert-file-to-list "savedQueryHistograms/colorHistogram_14.txt"))))

   (division x y)
   )

(loop-through-dataset (convert-directory "imageDataset2_15_20")(convert-file-to-list "savedQueryHistograms/colorHistogram_4.txt"))