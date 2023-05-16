#lang eopl

;; interpreter for the PROC language, using the procedural
;; representation of procedures.



(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; run : String -> ExpVal
(define run
  (lambda (s)
    (value-of-program (scan&parse s))))

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      
      (const-exp (num) (num-val num))
      
      
      (var-exp (var) (apply-env env var))
      
     
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      
      
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      
     
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      
      
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
      
      ;;--------------------------------------------------
      ; INSERT YOUR CODE HERE
      ; Write the required expressions starting from here
      ;;-------------------------------------------------

      ;; Create a new empty queue
      (newqueue-exp ()
                    (expval->queue (make-queue)))

      ;; Push an element to the back of the queue
      (queue-push-exp (queue elem)
                      (let ((q (expval->queue (value-of queue env)))
                            (val (value-of elem env)))
                        (enqueue val q)
                        (void-val)))

      ;; Pop an element from the front of the queue
      (queue-pop-exp (queue)
                     (let ((q (expval->queue (value-of queue env))))
                       (if (queue-empty? q)
                           (begin
                             (display "Warning: Queue is empty\n")
                             (newqueue-exp))
                           (queue-val (dequeue q)))))


      ;; Peek at the element at the front of the queue
      (queue-peek-exp (queue)
                      (let ((q (expval->queue (value-of queue env))))
                        (if (queue-empty? q)
                            (begin
                              (display "Warning: Queue is empty\n")
                              (num-exp 2000))
                            (num-exp (front q)))))

      ;; Push multiple elements to the back of the queue
      (queue-push-multi-exp (queue elems)
                            (let ((q (expval->queue (value-of queue env)))
                                  (vals (eval-list elems env)))
                              (for-each (lambda (val) (enqueue val q)) vals)
                              (void-val)))
      
      ;; Pop multiple elements from the front of the queue
      (queue-pop-multi-exp (queue n)
                           (let* ((q (expval->queue (value-of queue env)))
                                  (count (value-of n env)))
                             (if (> count (queue-size q))
                                 (begin
                                   (display "Warning: Queue is smaller than n\n")
                                   (newqueue-exp))
                                 (queue-val (make-queue (list->queue (take (queue->list q) count)))))))

      ;; Merge two queues
      (queue-merge-exp (queue1 queue2)
                       (let ((q1 (expval->queue (value-of queue1 env)))
                             (q2 (expval->queue (value-of queue2 env))))
                         (queue-val (make-queue (append (queue->list q1) (queue->list q2))))))


      ;;-------------------------------------------------
      
      )))

;;-----------------------------------------
; INSERT YOUR CODE HERE
; you may use this area to define helper functions
;;-----------------------------------------

(define (expval->queue expval)
  (if (queue? (expval-val expval))
      (expval-val expval)
      (error "Expected a queue, but got" expval)))

(define (queue-push-exp queue exp)
  (let ((q (expval->queue (value-of queue env)))
        (elem (value-of exp env)))
    (set! q (cons elem q))
    (make-expval 'void '())))

(define (make-queue)
  '())

(define (queue-empty? queue)
  (null? queue))

(define (front queue)
  (if (null? queue)
      (error "Queue is empty")
      (car queue)))

(define (dequeue queue)
  (if (null? queue)
      (error "Queue is empty")
      (begin
        (set! queue (cdr queue))
        queue)))


;;-----------------------------------------

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var val saved-env))))))
