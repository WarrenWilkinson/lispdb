(defpackage :util.encrypt 
  (:use :common-lisp :sb-md5 :util.util :util.blowfish)
  (:import-from :sb-ext string-to-octets octets-to-string with-pinned-objects vector-sap sap-ref-8 sap-ref-32 sap+ sap=)
  (:import-from :sb-kernel system-area-ub8-copy)
  (:export encrypt decrypt encrypt-string decrypt-string pp-md5 cr-md5 md5sum-sequence make-blowfish-key
	   passhash))

(in-package :util.encrypt)

(flet ((map-buf (fn byte-array)
	 (assert (zerop (boole boole-and #b00000111 (length byte-array)))) ;; Must be divisible by 8
	 ;;(format *loop-io* "~%MAP-BUF ~s" byte-array)
	 (with-pinned-objects (byte-array)
	   (let* ((start (vector-sap byte-array))
		  (end (sap+ start (length byte-array)))
		  (buf (make-blowfish-buffer :left 0 :right 0)))
	     (do ((sap start (sap+ sap 8)))
		 ((sap= sap end) byte-array)
	       (setf (buffer-left buf) (sap-ref-32 sap 0) (buffer-right buf) (sap-ref-32 sap 4))
	       (funcall fn buf)
	       (setf (sap-ref-32 sap 0) (buffer-left buf) (sap-ref-32 sap 4) (buffer-right buf)))))))
  (defun encrypt (key seq) (map-buf #'(lambda (buf) (blowfish-encrypt buf key)) seq))
  (defun decrypt (key seq) (map-buf  #'(lambda (buf) (blowfish-decrypt buf key)) seq)))

(flet ((prepend/pad8 (source value)
	 (let ((dest(make-array(ash(ceiling (1+ (length source)) 8) 3) :element-type '(unsigned-byte 8) :initial-element 0)))
	   (with-pinned-objects (source dest)
	     (system-area-ub8-copy (vector-sap source) 0 (vector-sap dest) 1 (length source))
	     (setf (sap-ref-8 (vector-sap dest) 0) value))
	   dest)))
  (defun encrypt-string (key string &key (external-format :utf8))
    ;; 0 padded to nearest 8 bytes, and prepended with 1 byte to store the amount of padding.
    (let ((enc (string-to-octets string :external-format external-format)))
      (encrypt key (prepend/pad8 enc (- 7 (mod (length enc) 8)))))))
(defun decrypt-string (key seq &key (external-format :utf8))
  (let ((sto (decrypt key seq)))
    (octets-to-string sto :external-format external-format :start 1 :end (- (length sto) (aref sto 0)))))

;; PrePend md5 and CheckRemove md5.
(defun pp-md5 (byte-array) (concatenate '(vector (unsigned-byte 8)) (md5sum-sequence byte-array) byte-array))
(defun cr-md5 (byte-array)
  ;(format *loop-io* "~%CR: ~s" byte-array)
  (or (every #'= (md5sum-sequence byte-array :start 16) byte-array) (error "MD5 checksum failed in remove-md5."))
  (subseq byte-array 16))

(let ((key (make-blowfish-key (util.blowfish:split-64-bit 5220413953771391049))))
  (defun passhash (string)
    (coerce (url-base64 (md5sum-sequence (encrypt-string key (concatenate 'string "milk" string "salt")))) 'string)))
