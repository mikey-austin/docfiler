;;; Commentary:

;; This module defines support routines for working use in the
;; docfiler unit tests.

;;; Code:

(define-module (docfiler test)
  #:use-module (docfiler utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (test-make-path
            test-setup-gpg-keychain
            test-cleanup-paths))

(define (test-make-path)
  "Make a temporary path string."
  (join-paths
   "/tmp" (string-append
           "test_" (number->string (random 1000000000)))))

(define (test-setup-gpg-keychain . recipients)
  "Create a new gpg keychain loaded with the specified recipients"
  (let ((new-gpg-home (test-make-path)))
    (mkdir new-gpg-home)
    (for-each
     (lambda (recipient)
       (system (format #f "gpg2 --homedir ~a --batch --yes --quick-gen-key --passphrase '' ~a"
                       new-gpg-home recipient))
       (let ((long-key-id (get-gpg-long-key-id recipient)))
         (system (format #f "echo ~a:6: |gpg2 --import-ownertrust" long-key-id))))
     recipients)
    new-gpg-home))

(define (make-key-cmd recipient)
  (format
   #f "gpg2 --list-keys --keyid-format LONG --with-colons |awk -F: '/~a/ {print $8}'"
   recipient))

(define (get-gpg-long-key-id recipient)
  (let* ((port (open-input-pipe (make-key-cmd recipient)))
         (key-id (read-line port)))
    (close-pipe port)
    key-id))

(define (test-cleanup-paths . paths)
  "Recursively delete all files & dirs in the specified paths."
  (for-each
   (lambda (path)
     (nftw path
           (lambda (filename statinfo flag base level)
             (case flag
               ((directory-processed) (rmdir filename))
               ((regular) (delete-file filename)))
             #t)
           'depth))
   paths))

;;; test.scm ends here.
