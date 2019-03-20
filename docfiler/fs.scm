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
  #:use-module (oop goops)
  #:use-module (ice-9 ports)
  #:use-module (ice-9 regex)
  #:export (<doc-fs>
            join-paths
            fs-make-abs-path
            fs-write-abs-path
            fs-read-abs-path
            fs-set-prop
            fs-get-prop
            fs-save-props
            fs-load-props))

(define-class <doc-fs> ()
  (base-path)
  (in-port #:init-value open-input-file)
  (out-port #:init-value open-output-file))

(define-method (fs-read-abs-path (fs <doc-fs>)
                                 (abs-path <string>))
  (if (file-exists? abs-path)
      (let* ((in-port ((slot-ref fs 'in-port) abs-path))
             (obj (read in-port)))
        (close-port in-port)
        obj)
      '()))

(define-method (fs-write-abs-path (fs <doc-fs>)
                                  (abs-path <string>)
                                  (obj <list>))
  (let ((out-port ((slot-ref fs 'out-port) abs-path)))
    (write obj out-port)
    (close-port out-port)))

(define (join-paths . paths)
  (regexp-substitute/global #f "/+" (string-join paths "/") 'pre "/" 'post))

(define-method (fs-make-abs-path (fs <doc-fs>)
                                 (path <string>))
  (unless (file-exists? (slot-ref fs 'base-path))
    (mkdir (slot-ref fs 'base-path)))
  (let ((abs-path (join-paths (slot-ref fs 'base-path) path)))
    (unless (file-exists? abs-path)
      (mkdir abs-path))
    abs-path))

(define meta-filename "meta.gpg")

(define-method (fs-load-props (fs <doc-fs>)
                              (path <string>))
  (fs-read-abs-path fs (join-paths (fs-make-abs-path fs path) meta-filename)))

(define-method (fs-save-props (fs <doc-fs>)
                              (path <string>)
                              (props <list>))
  (fs-write-abs-path fs
                     (join-paths (fs-make-abs-path fs path) meta-filename)
                     props))

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

;; fs.scm end
