;; Commentary:

;; This module contains the classes and functions to maintain a docfiler
;; filesystem database structure.
;;
;; The structure of the filesystem is as follows:
;;
;;   /base/
;;     \_ 2019/
;;        \_03/
;;          \_20/
;;            \_foo/
;;              \_meta.gpg
;;               _attachments/
;;                \_attachment1.png.gpg
;;                 _attachment2.pdf.gpg
;;            \_bar/
;;              \_meta.gpg
;;               _attachments/
;;                \_...
;;

;; Code:

(define-module (docfiler fs)
  #:use-module (docfiler utils)
  #:use-module (oop goops)
  #:use-module (ice-9 ports)
  #:use-module (scheme documentation)
  #:use-module (ice-9 ftw)
  #:export (<doc-fs>
            fs-make-abs-path
            fs-write-abs-path
            fs-read-abs-path
            fs-for-each
            fs-set-prop
            fs-get-prop
            fs-save-props
            fs-load-props))

(define-class <doc-fs> ()
  (base-path #:init-keyword #:base-path)
  (in-port #:init-value open-input-file #:init-keyword #:in-port)
  (out-port #:init-value open-output-file #:init-keyword #:out-port)
  (port-closer #:init-value close-port #:init-keyword #:port-closer)
  (meta-filename #:init-value "meta" #:init-keyword #:meta-filename))

(define-method (fs-read-abs-path (fs <doc-fs>)
                                 (abs-path <string>))
  (if (file-exists? abs-path)
      (let* ((in-port ((slot-ref fs 'in-port) abs-path))
             (obj (read in-port)))
        ((slot-ref fs 'port-closer) in-port)
        obj)
      '()))

(define-method (fs-write-abs-path (fs <doc-fs>)
                                  (abs-path <string>)
                                  (obj <list>))
  (let ((out-port ((slot-ref fs 'out-port) abs-path)))
    (write obj out-port)
    ((slot-ref fs 'port-closer) out-port)))

(define-method (fs-make-abs-path (fs <doc-fs>)
                                 (path <string>))
  (unless (file-exists? (slot-ref fs 'base-path))
    (mkdir-p (slot-ref fs 'base-path)))
  (let ((abs-path (join-paths (slot-ref fs 'base-path) path)))
    (unless (file-exists? abs-path)
      (mkdir-p abs-path))
    abs-path))

(define-method (fs-load-props (fs <doc-fs>)
                              (path <string>))
  (let ((abs-path (join-paths
                   (fs-make-abs-path fs path)
                   (slot-ref fs 'meta-filename))))
    (fs-read-abs-path fs abs-path)))

(define-method (fs-save-props (fs <doc-fs>)
                              (path <string>)
                              (props <list>))
  (let ((abs-path (join-paths
                   (fs-make-abs-path fs path)
                   (slot-ref fs 'meta-filename))))
    (fs-write-abs-path fs abs-path props)))

(define-method (fs-get-prop (fs <doc-fs>)
                            (path <string>)
                            (prop-name <string>))
  (let ((props (fs-load-props fs path)))
    (cdr (assoc prop-name props))))

(define-method (fs-set-prop (fs <doc-fs>)
                            (path <string>)
                            (prop-name <string>)
                            (prop-value <string>))
  (let ((props (fs-load-props fs path)))
    (fs-save-props fs path (assoc-set! props prop-name prop-value))))

(define-generic-with-docs fs-for-each
  "\
Iterate over each file path relative to the base path of the
fs instance.
")

(define-method (fs-for-each (fs <doc-fs>)
                            (proc <procedure>))
  (nftw (slot-ref fs 'base-path)
        (lambda (filename statinfo flag base level)
          (if (equal? (basename filename) (slot-ref fs 'meta-filename))
              (proc (extract-rel-file-path fs filename)))
          #t)))

(define-method (extract-rel-file-path (fs <doc-fs>)
                                      (filename <string>))
  (let* ((file-dir (dirname filename))
         (rel-path (string-replace
                    file-dir "" 0
                    (string-length
                     (string-trim-right (slot-ref fs 'base-path) #\/)))))
    (string-trim rel-path #\/)))

;; fs.scm end
