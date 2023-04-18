;#!r7rs
;;;;
(import
  (scheme base)
  (scheme write))
;(install-r7rs!)
;1. a  String
;2. l  Starting index of the string
;3. r Ending index of the string. 

(define (mainperm lst)
  (define retval '())        
  (define (perm lst stptr endptr)    
    (if (= stptr endptr)     
        (set! retval (cons lst retval))
      (begin                      
        (let loop ((idx stptr))
            (if (not (= idx endptr))                  
              (begin                                                                      
                (perm (swap stptr idx lst) (+ stptr 1) endptr)                        
                (loop (+ idx 1))))))))

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
  
  (perm lst 0 (length lst))

  retval)

(display (mainperm '(A B C D)))
