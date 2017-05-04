;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname median) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;Christopher Thompson
;cs4250 hw5
;date 04/05/2017

(define (median list)
  (cond
    ((= 0 (modulo (length list) 2)) (evenList (sort list <)))
    (else (oddList (sort list <)))
  )
  
)

(define (getItem n list)
  (if (or (> n (length list)) (< n 0))
    (error "[-]ERROR: out of bounds exception")
    
    (if (eq? n 0)
      (car list)
      (getItem (- n 1) (cdr list))
    )
  )
)

(define (sumList list)
  (cond
    ((null? list) 0)
    (else (+ (car list) (sumList (cdr list))))
  )
)

(define (getListAvg list)
  (/ (sumList list) (length list))
)

(define (oddList list)
  (getItem (floor (/ (length list) 2)) list)
) 

(define (evenList list)
  (cond
    ((< (diffNP1 list) (diffN list)) (getNP1 list))
    ((eq? (diffNP1 list) (diffN list)) (getNP1 list))
    (else(getN list))
  )
)

(define (diff x y)
  (abs (- x y))
)

(define (diffN list)
  (diff (getListAvg (sort list <)) (getItem (- (/ (length list) 2) 1) (sort list <)))
)

(define (diffNP1 list)
  (diff (getListAvg (sort list <)) (getItem (/ (length list) 2) (sort list <)))
)

(define (getN list)
  (getItem (- (/ (length list) 2) 1) (sort list <))
)

(define (getNP1 list)
  (getItem (/ (length list) 2) (sort list <))
)