#!r7rs
;;;;
(import
  (scheme base)
  (scheme write))

;1. a  String
;2. l  Starting index of the string
;3. r Ending index of the string. 

(define (swap a b lst)
  (define len (length lst))
    (let loop ((idx 0)(retlst '()))
      (if (= idx len)           
        retlst
        (loop (+ idx 1) (append retlst
          `(,(cond 
          ((= idx a) (list-ref lst b))
          ((= idx b) (list-ref lst a))
          ((list-ref lst idx)))))))))

(define (perm lst stptr endptr)     
  (if (= stptr endptr)
    (begin
      (display lst)(newline))
    (begin                      
      (let loop ((idx stptr))
          (if (not (= idx endptr))                  
            (begin                                                                      
              (perm (swap stptr idx lst) (+ stptr 1) endptr)                        
              (loop (+ idx 1))))))))

(define (main lst)
  (perm lst 0 (length lst)))

(main '(A B C D))
