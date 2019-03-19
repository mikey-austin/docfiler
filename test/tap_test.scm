(use-modules (docfiler tap)
             (oop goops))

;; Make a dummy test that we can test.
(define dummy-test (make <tap-test-collection>))

(register-test dummy-test "a successful test" (is-ok #t))
(register-test dummy-test "a failed test" (is-ok #f))
(register-skip dummy-test "a skipped test" (is-ok #f) "skipped reason")
(register-todo dummy-test "another failed test" (is-ok #f) "todo reason")

 ;; The real test.
(define test (make <tap-test-collection>))

(define saved-output-port (current-output-port))
(define output-capture-port (open-output-string))
(set-current-output-port output-capture-port)
(run-tests dummy-test)
(set-current-output-port saved-output-port)

;; Test the results from running the dummy tests
(register-test test "successful number of tests ok" (is-eq 2 (slot-ref dummy-test 'num-ok)))

(register-test test "failed number of tests ok" (is-eq 2 (slot-ref dummy-test 'num-failed)))

(let ((captured (get-output-string output-capture-port)))
  (register-test test "correct TAP header" (is-eq 0 (string-contains captured "1..4")))
  (register-test test "correct SKIPPED reason"
                 (is-ok (> (string-contains captured "# SKIP skipped reason") 0)))
  (register-test test "correct TODO reason"
                 (is-ok (> (string-contains captured "# TODO todo reason") 0))))

(run-tests test)
