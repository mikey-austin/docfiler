(use-modules (docfiler gpg)
             (docfiler tap)
             (oop goops)
             (ice-9 popen))

(define test (make <tap-test-collection>))

(register-test
 test "the encrypt command is ok"
 (is-eq
  '("/usr/bin/gpg2" "-eaq" "-r" "jack" "-r" "stevenson"
    "--batch" "--yes" "--output" "/tmp/myfile")
  (gpg-encrypt-cmd '("jack" "stevenson") "/tmp/myfile" #:gpg-prog "/usr/bin/gpg2")))

(register-test
 test "the decrypt command is ok"
 (is-eq
  '("/usr/bin/gpg2" "--batch" "--yes" "-dq" "/tmp/myfile")
  (gpg-decrypt-cmd "/tmp/myfile" #:gpg-prog "/usr/bin/gpg2")))

(run-tests test)
