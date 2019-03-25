(use-modules (docfiler utils)
             (docfiler tap))

(define test (make <tap-test-collection>))

(register-test
 test "ensure path joining works"
 (is-eq "a/b/c/d" (join-paths "a////" "b" "//c////" "d")))

(run-tests test)
