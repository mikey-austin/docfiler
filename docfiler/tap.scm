;;; Commentary:

;; Super basic TAP class (test anything protocol).

;;; Code:

(define-module (docfiler tap)
  #:use-module (ice-9 textual-ports)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:export (<tap-test-collection>
            run-tests
            register-test
            register-todo
            register-skip
            is-ok
            is-eq))

(define-class <tap-test-collection> ()
  (tests #:init-value '())
  (num-ok #:init-value 0)
  (num-failed #:init-value 0))

(define-method (test-ok (t <tap-test-collection>)
                        (desc <string>)
                        (test-number <number>))
  (slot-set! t 'num-ok (+ 1 (slot-ref t 'num-ok)))
  (format #t "ok ~a - ~a~%" test-number desc))

(define-method (test-failed (t <tap-test-collection>)
                            (desc <string>)
                            (test-number <number>))
  (slot-set! t 'num-failed (+ 1 (slot-ref t 'num-failed)))
  (format #t "not ok ~a - ~a~%" test-number desc))

(define-method (print-tap-header (t <tap-test-collection>))
  (let ((num-tests (length (slot-ref t 'tests))))
    (format #t "1..~a~%" num-tests)))

(define-method (next-test-number (t <tap-test-collection>))
  (+ 1 (length (slot-ref t 'tests))))

(define (register-with-diag t desc test-code diag explanation)
  (register-test t
                 (format #f "~a # ~a ~a" desc diag explanation)
                 test-code))

;; Exported routines.

(define (register-skip t desc test-code explanation)
  "Do not run the supplied test code, but instead output a
SKIP diagnostic and mark as successful"
  (register-with-diag t desc (lambda () #t) "SKIP" explanation))

(define (register-todo t desc test-code explanation)
  (register-with-diag t desc test-code "TODO" explanation))

(define-method (register-test (t <tap-test-collection>)
                              (desc <string>)
                              (test-code <procedure>))
  "Register a deferred test to be run later by the `run-tests'
routine."
  (let ((test-number (next-test-number t)))
    (slot-set! t 'tests
               (cons (lambda ()
                       (if (test-code)
                           (test-ok t desc test-number)
                           (test-failed t desc test-number)))
                     (slot-ref t 'tests)))))

(define-method (run-tests (t <tap-test-collection>))
  "Execute all deferred registered tests and keep track of the
number of failed and successful tests. Also output the results
in TAP output."
  (print-tap-header t)
  (for-each (lambda (t) (t)) (reverse (slot-ref t 'tests))))

(define-syntax is-eq
  (syntax-rules ()
    ((_ expected expr)
     (lambda ()
       (equal? expected expr)))))

(define-syntax is-ok
  (syntax-rules ()
    ((_ expr)
     (is-eq #t expr))))

;;; tap.scm ends here
