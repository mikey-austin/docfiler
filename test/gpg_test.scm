(use-modules (docfiler gpg)
             (docfiler tap)
             (oop goops)
             (ice-9 popen))

(define test (make <tap-test-collection>))

(register-test
 test "the encrypt command is ok"
 (is-eq
  '("/usr/bin/gpg2" "-ea" "-r" "jack" "-r" "stevenson"
    "--batch" "--yes" "--output" "/tmp/myfile")
  (gpg-encrypt-cmd '("jack" "stevenson") "/tmp/myfile")))

(register-test
 test "the decrypt command is ok"
 (is-eq
  '("/usr/bin/gpg2" "--batch" "--yes" "-d" "/tmp/myfile")
  (gpg-decrypt-cmd "/tmp/myfile")))

(run-tests test)
