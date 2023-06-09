(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  (require racket/trace)
  
  (provide value-of-program value-of instrument-let instrument-newref)

  ;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

  ;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
                   (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (num-val
                       (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
                   (let ((val1 (value-of exp1 env)))
                     (let ((num1 (expval->num val1)))
                       (if (zero? num1)
                           (bool-val #t)
                           (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
                (let ((val1 (value-of exp1 env)))
                  (if (expval->bool val1)
                      (value-of exp2 env)
                      (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
                 (let ((val1 (value-of exp1 env)))
                   (value-of body
                             (extend-env var val1 env))))
        
        (proc-exp (var body)
                  (proc-val (procedure var body env)))

        (call-exp (rator rand)
                  (let ((proc (expval->proc (value-of rator env)))
                        (arg (value-of rand env)))
                    (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
                    (value-of letrec-body
                              (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
                   (letrec 
                       ((value-of-begins
                         (lambda (e1 es)
                           (let ((v1 (value-of e1 env)))
                             (if (null? es)
                                 v1
                                 (value-of-begins (car es) (cdr es)))))))
                     (value-of-begins exp1 exps)))

        (newref-exp (exp1)
                    (let ((v1 (value-of exp1 env)))
                      (ref-val (newref v1))))

        (deref-exp (exp1)
                   (let ((v1 (value-of exp1 env)))
                     (let ((ref1 (expval->ref v1)))
                       (deref ref1))))

        (setref-exp (exp1 exp2)
                    (let ((ref (expval->ref (value-of exp1 env))))
                      (let ((v2 (value-of exp2 env)))
                        (begin
                          (setref! ref v2)
                          (num-val 23)))))

        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### value-of cases for new expressions, remember
        ; ###### that you need to use memory functionalities. 
        ; #####################################################

        ;; VECTOR-EXP
        ; newvector(length, value): initializes a vector of size length with the value value.
        (newvector-exp (length-exp val-exp)
                       (let ((len (expval->num (value-of length-exp env)))
                             (val (value-of val-exp env)))
                         (vector-val (make-vector len val) len)))
        
        ; update-vector(vec, index, value): updates the value of the vector vec at index index by value value.
        (update-vector-exp (exp1 exp2 exp3)
                           (let ((v1 (value-of exp1 env))
                                 (v2 (value-of exp2 env))
                                 (v3 (value-of exp3 env)))
                             (let ((vec (car (expval->vector v1)))
                                   (index (expval->num v2)))
                               (set-vector! vec index v3))))
        
        ; read-vector(vec, index): returns the element of the vector vec at index index.
        (read-vector-exp (exp1 exp2)
                         (let ((v1 (value-of exp1 env))
                               (v2 (value-of exp2 env)))
                           (let ((vec (car (expval->vector v1)))
                                 (index (expval->num v2)))
                             (vector-ref vec index))))
        
        ; length-vector(vec) returns the length of the vector vec.
        (length-vector-exp (exp1)
                           (let ((v1 (value-of exp1 env)))
                             (let ((vec (cdr (expval->vector v1))))
                               (num-val vec))))
        
        ; swap-vector(vec, index, index): swaps the values of the indexes in the vector vec.
        (swap-vector-exp (exp1 exp2 exp3)
                         (let ((v1 (value-of exp1 env))
                               (v2 (value-of exp2 env))
                               (v3 (value-of exp3 env)))
                           (let ((vec (car (expval->vector v1)))
                                 (index1 (expval->num v2))
                                 (index2 (expval->num v3)))
                             (let ((temp (vector-ref vec index1)))
                               (set-vector! vec index1 (vector-ref vec index2))
                               (set-vector! vec index2 temp)))))

        
        ; copy-vector(vec): initializes an new vector with the same values of the given vector vec.
        (copy-vector-exp (exp)
                         (let ((vec (car (expval->vector (value-of exp env)))))
                           (copy-vector vec (- (cdr (expval->vector (value-of exp env))) 1))  ))


        ; vec-mult-exp (vec1, vec2): takes two vectors, calculates their pairwise multiplication and outputs a new vector. If the sizes of the vectors are not equal, it throws an error
        (vec-mult-exp (exp1 exp2)
                      (let ((vec1 (expval->vector (value-of exp1 env)))
                            (vec2 (expval->vector (value-of exp2 env))))
                        (let ((result (vec-mult (car vec1) (car vec2) (- (cdr vec1) 1))))
                          (if (not (eq? (cdr vec1) (cdr vec2)))
                              (eopl:error  "vectors must have the same length") result))))




        ;; QUEUE-EXP
        ; newqueue(L) returns an empty queue with max-size L.
        (newqueue-exp (L)
                      (let ((max-size (value-of L env)))
                        (let ((queue (make-vector max-size '())))
                          (vector-val queue))))




        ; enqueue(q, val) adds the element val to the queue q. If the queue is full it throws a stack overflow error.
        (enqueue-exp (queue-exp val-exp)
                     (let ((v1 (value-of queue-exp env))
                           (val (value-of val-exp env)))
                       (let ((size (vector-ref (expval->vector v1) 0))
                             (elements (vector-ref (expval->vector v1) 1)))
                         (if (= (length elements) size)
                             (eopl:error "Queue overflow")
                             (begin
                               (set-vector! (expval->vector v1) 1 (append elements (list val)))
                               (expval->num (length elements)))))))

        ; dequeue(q) removes the first element of the queue q and returns its value.
        (dequeue-exp (queue-exp)
                     (let ((v1 (value-of queue-exp env)))
                       (let ((elements (vector-ref (expval->vector v1) 1)))
                         (if (null? elements)
                             (num-val -1)
                             (let ((first-element (car elements)))
                               (begin
                                 (set-vector! (expval->vector v1) 1 (cdr elements))
                                 first-element))))))

        ; queue-size(q) returns the number of elements in the queue q.
        (queue-size-exp (queue-exp)
                        (let ((v1 (value-of queue-exp env)))
                          (length (vector-ref (expval->vector v1) 1))))

        ; peek-queue(q) returns the value of the first element in the queue q without removal.
        (peek-queue-exp (queue-exp)
                        (let ((v1 (value-of queue-exp env)))
                          (let ((elements (vector-ref (expval->vector v1) 1)))
                            (if (null? elements)
                                (num-val -1)
                                (car elements)))))

        ; queue-empty?(q) returns true if there is no element inside the queue q and false otherwise.
        (queue-empty-exp (queue-exp)
                         (let ((v1 (value-of queue-exp env)))
                           (let ((elements (vector-ref (expval->vector v1) 1)))
                             (null? elements))))

        ; print-queue(q) prints the elements in the queue q.
        (print-queue-exp (queue-exp)
                         (let ((v1 (value-of queue-exp env)))
                           (let ((elements (vector-ref (expval->vector v1) 1)))
                             (for-each (lambda (element)
                                         (display element)
                                         (display " "))
                                       elements)
                             (newline))))

      
        ; #####################################################
        )))

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE
  (define read-vector
    (lambda (vector length)
      (let ((expval (vector-ref vector length)))
        (if (null? expval) (read-vector vector (+ length 1))
            (let ((val (expval->num expval)))
              (if (= val -1) '() (cons val (read-vector vector (+ length 1)))))))))

  (define print-vector
    (lambda (lst)
      (if (null? lst) '()
          (letrec ((print-lst
                    (lambda (lst)
                      (display (car lst))
                      (display " ")
                      (if (null? (cdr lst))
                          (display "")
                          (print-lst (cdr lst))))))
            (print-lst lst)))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
                   (let ((r arg))
                     (let ((new-env (extend-env var r saved-env)))
                       (when (instrument-let)
                         (begin
                           (eopl:printf
                            "entering body of proc ~s with env =~%"
                            var)
                           (pretty-print (env->list new-env))
                           (eopl:printf "store =~%")
                           (pretty-print (store->readable (get-store-as-list)))
                           (eopl:printf "~%")))
                       (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
       (lambda (p)
         (cons
          (car p)
          (expval->printable (cadr p))))
       l)))
 
  )
  


  
