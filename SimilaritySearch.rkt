#lang racket
(require racket/dir)
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
;function to convert a directory into a list;
(define (convert-directory dirName)
  (directory-list dirName)
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

   (/ x y)
   )