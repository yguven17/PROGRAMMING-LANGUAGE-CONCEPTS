(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?
  (require "store.scm")                 ; for reference?

  (provide (all-defined-out))               ; too many things to list

  ;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

  ;;; an expressed value is either a number, a boolean, a procval, or a
  ;;; reference. 

  (define-datatype expval expval?
    (num-val
     (value number?))
    (bool-val
     (boolean boolean?))
    (proc-val 
     (proc proc?))
    (ref-val
     (ref reference?))
    ; #####################################################
    ; ###### ENTER YOUR CODE HERE
    ; ###### add a new value type for your vectors (and possible for queues)
    ; #####################################################
    (vector-val 
     (ref reference?)
     (len number?)
     )
    (pair-val
     (pair pair?))
    
    ; #####################################################
    )

  ;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
        (num-val (num) num)
        (else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
        (bool-val (bool) bool)
        (else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
        (proc-val (proc) proc)
        (else (expval-extractor-error 'proc v)))))

  (define expval->ref
    (lambda (v)
      (cases expval v
        (ref-val (ref) ref)
        (else (expval-extractor-error 'reference v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                  variant value)))

  ;; HINT if you need extractors, add them here


  (define expval->vector
    (lambda (v)
      (cases expval v
        (vector-val (ref len) (cons ref len))
        (else (expval-extractor-error 'vector v)))))
  
  

  ;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ; #####################################################
  ; ###### ENTER YOUR CODE HERE
  ; ###### you might want to add a new datatype for vectors here similar 
  ; ###### to mutable pairs.
  ; #####################################################
  (define-datatype vector vector?
    [an-vector [ref reference?]
               [length (lambda (x)
                         (and (integer? x)
                              (positive? x)))]])
  
 
    
  (define make-vector
    (lambda (length value)
      (letrec ((save-to-store
                (lambda (length)
                  (if (= length 0)
                      '()
                      (let ((new (newref value)))
                        (save-to-store (- length 1)))))))
        (let ((first (newref value)))(save-to-store (- length 1))first))))

  (define vector-ref
    (lambda (vector index)
     
      
      (begin
            
        (deref (+ vector index)))
          
      ))
  
  (define set-vector!
    (lambda (vector index val)
      (setref! (+ vector index) val)))

  (define length-vector
    (lambda(vector)
      (let ((expval (deref vector)))
        (if (null? expval) (length-vector (+ vector 1))
            (let ((val (expval->num expval)))
              (if (= val -1) 0 (+ 1 (length-vector (+ vector 1)))))))))

  (define vec-mult
      
    (lambda (vec1 vec2 num)
      (let ((resvec (make-vector (+ 1 num) 0)))
        (letrec((vec-mult-helper
                 (lambda (v1 v2 n )
                   (if (> n 0 )
                       (begin
                         (set-vector! resvec n (num-val (* (expval->num  (vector-ref v1 n)) (expval->num  (vector-ref v2 n)))))
                         (vec-mult-helper  v1  v2 (- n 1) )
                         )
                       (set-vector! resvec n (num-val (* (expval->num  (vector-ref v1 n)) (expval->num  (vector-ref v2 n)))))
                       ))))
        
          (vec-mult-helper vec1 vec2 num)
        
          (vector-val resvec (+ 1 num))
        
          ))))

      
      
  (define copy-vector
    (lambda (vec1 num)
      (let ((resvec (make-vector (+ 1 num) 0)))
        (letrec((vec-copy-helper
                 (lambda (v1  n )
                   (if (> n 0 )
                       (begin
                         (set-vector! resvec n (num-val (expval->num  (vector-ref v1 n))))
                         (vec-copy-helper  v1   (- n 1) )
                         )
                       (set-vector! resvec n (num-val (expval->num  (vector-ref v1 n) )))
                       ))))
        
          (vec-copy-helper vec1  num)
        
          (vector-val resvec (+ 1 num))
        
          ))))
  
  ; #####################################################

  (define-datatype proc proc?
    (procedure
     (bvar symbol?)
     (body expression?)
     (env environment?)))
  
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
     (bvar symbol?)
     (bval expval?)
     (saved-env environment?))
    (extend-env-rec*
     (proc-names (list-of symbol?))
     (b-vars (list-of symbol?))
     (proc-bodies (list-of expression?))
     (saved-env environment?)))

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
        (empty-env () '())
        (extend-env (sym val saved-env)
                    (cons
                     (list sym (expval->printable val))
                     (env->list saved-env)))
        (extend-env-rec* (p-names b-vars p-bodies saved-env)
                         (cons
                          (list 'letrec p-names '...)
                          (env->list saved-env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (cases expval val
        (proc-val (p)
                  (cases proc p
                    (procedure (var body saved-env)
                               (list 'procedure var '... (env->list saved-env)))))
        (else val))))

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE
    

  )
