;; blowfish -- implements the blowfish encryption algorithm.
;; This algorithm was invented by Bruce Schneier, and is described at
;; http://www.counterpane.com/bfsverlag.html
;; The blowfish algorithm is patent and royalty free.
;;
;; Author: Alain Picard <apicard@optushome.com.au>
;;         (also alain.picard@memetrics.com)
;;
;; Version: 0.6
;; File: $Id: blowfish.lisp,v 1.6 2004/03/05 09:27:38 ap Exp $
;;
;; License:
;; This code is placed under the Lesser GNU Public License (LGPL)
;; (see http://www.fsf.org/licenses/lgpl.html) as
;; clarified for Lisp by Franz when they released AllegroServe (see
;; http://allegroserve.sourceforge.net/license-allogroserve.txt)
;;
;; What this clarification basically means is that compiling this
;; file and loading it into your lisp image, either at time of
;; delivery or runtime, does not make your program a derivative of
;; this one.  YOU ARE EXPRESSLY PERMITTED TO LOAD THIS FILE INTO
;; YOUR LISP IMAGE, AT ANY TIME, FOR ANY AND ALL (INCLUDING COMMERCIAL)
;; PURPOSES.  In particular, clause 5 of the LGPL is NOT invoked
;; by you embedding or loading this code, interpeted or compiled,
;; into your application.
;;
;; Of course, if you make modifications to this file, then the terms
;; of the LGPL hold, and you must redistribute the sources of this
;; file and your modifications with your application.
;;
;; Lastly, if you DO make useful changes to this code, I would
;; appreciate receiving the changes (though this NOT a requirement
;; of the license of this code.)
;;
;;
;; This software is "as is", and has no warranty of any kind.  The
;; author assumes no responsibility for the consequences of any use
;; of this software.
;;
;;
;; Notes
;; -----
;; Blowfish was designed to be efficient on 32bit machines when doing
;; modulo 2^32 arithmetic.  This ought to be possible (in a portable
;; way) in Lisp, but in practice it isn't, so this code is slow
;; (compared with C) and conses a lot.
;;
;; Pierre Mai has written a file, md5.lisp, which shows how to
;; dramatically improve performance on a specific implementation
;; (CMUCL).  I've tried to make such improvements in this code, and
;; doing it for other implementations is on the TODO list.  SBCL
;; supports efficient arithmetic natively.
;;
;; Another possible approach would be to rewrite this using
;; 2 pairs of 16bit words, which would produce efficient code
;; under most implementations (as fixnums are typically at
;; least 20 bits, nowadays)
;;
;; Many thanks to Christophe Rhodes <csr21@*NOSPAM*NOSPAM*cam.ac.uk>
;; Message-ID: <sqd681thyt.fsf@lambda.dyndns.org>
;; for some SBCL specific speedups which improve performance by a factor
;; of two, at least on my itty-bitty laptop.
;;
;;
;;
;; Documentation
;; -------------
;; Not a lot so far.  See the design notes embedded below, and
;; the doc strings for the functions exported by this package.
;;
;; The function RUN-TEST-HARNESS runs the BLOWFISH-ENCRYPT
;; and BLOWFISH-DECRYPT functions against all test vectors
;; published at http://www.counterpane.com/vectors.txt
;; If it returns OK, things are working properly.
;;
;; Performace
;; On a 433Mhz Celeron and CMUCL, I can get 200,000 blowfish-encrypt
;; per second, or about 1.5 Mb/sec of encryption (each round encrypting
;; 64 bits.)  This is way, way slower than what is claimed on
;; counterpane's web site, where they reach 8.3 Mb/s on a Pentium 150.
;; I guess that's what happens when you program in portable assembler.
;; :-)
;;
;; TODO
;; ----
;; * Implement a streams interface, for both ECB CBC modes
;;
;; * improve the documentation
;;
;; * improve performance by using low-level word-size arithmetic
;;   on implementations other than CMUCL.
;;
;;   either by rewriting for 16bit (making encrypted file unportable
;;   to other 32bit blowfish implementations) or going to FFI.


(defpackage :util.blowfish
  (:use :common-lisp)
  (:export #:blowfish-encrypt #:blowfish-decrypt
	   #:make-blowfish-buffer
	   #:make-blowfish-key
	   #:split-64-bit
	   #:blowfish-encrypt-string
	   #:buffer-left #:buffer-right))

(in-package :util.blowfish)

;;;; Math

;; This stuff was obtained from Pierre Mai's md5.lisp
;; code, and you'd think he'd know about optimizing CMUCL.
;; Weird.

(eval-when (:compile-toplevel :load-toplevel :execute)

(deftype unsigned-byte-32 ()
  "Corresponds to the 32bit quantity word of the MD5 Spec
   and BLOWFISH algorithm"
  `(unsigned-byte 32))

(deftype blowfish-array ()
  `(simple-array unsigned-byte-32 (*)))

(defun make-blowfish-array (entries)
  "Constructs a specialized BLOWFISH array."
  (dolist (entry entries)
    (assert (typep entry 'unsigned-byte-32)
	    nil
	    "The supplied number ~A does not fit in 32 bits." entry))
  (make-array (length entries)
	      :element-type 'unsigned-byte-32
	      :initial-contents entries))

) ; eval-when
;; This cool hack came from Pierre's md5.lisp.
;; I'd have never found it.  I wonder if there are
;; similar hacks for Lispworks.
;; It alone speeds up encryption a factor of 3,
;; as well as diminishing the consing a factor of 3.
;;
#+cmu
(define-compiler-macro mod32-add (a b)
  `(ext:truly-the unsigned-byte-32 (+ ,a ,b)))

;;; SBCL has a limit on the number of inline expansions per
;;; component. This version is also compiled faster.
#+sbcl
(define-compiler-macro mod32-add (a b)
  `(ldb (byte 32 0) (+ ,a ,b)))

#-(or cmu sbcl)
(declaim (inline mod32-add)
         (ftype (function (unsigned-byte-32 unsigned-byte-32) unsigned-byte-32) mod32-add))

#-(or cmu sbcl)
(defun mod32-add (a b)
  (declare (type unsigned-byte-32 a b)
	   (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (ldb (byte 32 0) (+ a b)))

#+cmu
(define-compiler-macro fast-32bit-xor (a b)
  `(ext:truly-the unsigned-byte-32 (kernel:32bit-logical-xor ,a ,b)))

#+sbcl
(define-compiler-macro fast-32bit-xor (a b)
  `(logxor ,a ,b))

#-(or cmu sbcl)
(declaim (inline fast-32bit-xor))
#-(or cmu sbcl)
(defun fast-32bit-xor (a b)
  (declare (type unsigned-byte-32 a b)
	   (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (the unsigned-byte-32
    (logxor a b)))

(defmacro xorf (place num)
  "Modify PLACE to be PLACE XOR NUM."
  `(setf ,place (fast-32bit-xor ,place ,num)))

(declaim (inline split-64-bit))
(defun split-64-bit (number)
  "Return a vector containing two 32 bit numbers
   the left 4 bytes and the right 4 bytes."
  (vector (ash (logand #xFFFFFFFF00000000 number) -32)
	  (logand #xFFFFFFFF number)))

(declaim (inline join-32-bit))
(defun join-32-bit (left right)
  (declare (type unsigned-byte-32 left right))
  (+ (ash left 32)
     right))

;; eventually, both of these can probably be macros
(defun get-32bit-num (stream)
  "Read 1 32 bit number by parsing 4 characters of STREAM.
   If necessary, pad with NULLs.  This assumes the stream
   contains things who'se char-codes are 8 bits wide."
  (logxor (ash (char-code (read-char stream nil #\Null)) 24)
	  (ash (char-code (read-char stream nil #\Null)) 16)
	  (ash (char-code (read-char stream nil #\Null)) 08)
	  (ash (char-code (read-char stream nil #\Null)) 00)))

(defun string->key-vec (string)
  "Turn a string into a key vector"
  (with-input-from-string (stream string)
    (coerce (loop repeat (ceiling (length string) 4)
		  collect (get-32bit-num stream))
	    'vector)))


(defun split-number (number bits)
  "Return a list of numbers, each of which is BITS long.
   e.g. (split-number #x12345678 16) -> (#x12 #x34 #x56 #x78)"
  ;; FIXME: Note sure this works if bits is not an
  ;; even multiple of 8.
  (loop with byte = (byte bits 0)
	repeat (ceiling (integer-length number) bits)
	for pos from 0 by bits
	collect (ldb byte number)
	do
	(setf number (ash number (- bits)))))

(defmacro with-numeric-fields (((&rest symbols) number &key (size 8))
			       &body body)
  (assert (and (every #'symbolp symbols)
	       (= (length symbols)
		  (length (remove-duplicates symbols)))))
  (let ((gnum (gensym)))
    `(let* ((,gnum ,number))
      (symbol-macrolet
	    (;; Make bindings
	     ,@(let ((pos 0))
		    (mapcar #'(lambda (sym)
				(prog1 (list sym `(ldb (byte ,size ,pos) ,gnum))
				  (incf pos size)))
			    (reverse symbols))))
      ,@body))))

;;;; Blowfish Constants.
;;
;; These values taken from constants.txt,
;; at counterpane.com, in the description of blowfish.
;; They're actually the digits of pi, in Hex.
;;

(defmacro defconstant* (symbol form)
  `(defconstant ,symbol
    (if (boundp ',symbol)
	,symbol
	,form)))

(defconstant* +initial-p-array+
  (make-blowfish-array
   '(#x243f6a88 #x85a308d3 #x13198a2e #x03707344 #xa4093822 #x299f31d0
     #x082efa98 #xec4e6c89 #x452821e6 #x38d01377 #xbe5466cf #x34e90c6c
     #xc0ac29b7 #xc97c50dd #x3f84d5b5 #xb5470917 #x9216d5d9 #x8979fb1b)))

(defconstant* +initial-s-box-0+
  (make-blowfish-array
   '(#xd1310ba6 #x98dfb5ac #x2ffd72db #xd01adfb7 #xb8e1afed #x6a267e96
     #xba7c9045 #xf12c7f99 #x24a19947 #xb3916cf7 #x0801f2e2 #x858efc16
     #x636920d8 #x71574e69 #xa458fea3 #xf4933d7e #x0d95748f #x728eb658
     #x718bcd58 #x82154aee #x7b54a41d #xc25a59b5 #x9c30d539 #x2af26013
     #xc5d1b023 #x286085f0 #xca417918 #xb8db38ef #x8e79dcb0 #x603a180e
     #x6c9e0e8b #xb01e8a3e #xd71577c1 #xbd314b27 #x78af2fda #x55605c60
     #xe65525f3 #xaa55ab94 #x57489862 #x63e81440 #x55ca396a #x2aab10b6
     #xb4cc5c34 #x1141e8ce #xa15486af #x7c72e993 #xb3ee1411 #x636fbc2a
     #x2ba9c55d #x741831f6 #xce5c3e16 #x9b87931e #xafd6ba33 #x6c24cf5c
     #x7a325381 #x28958677 #x3b8f4898 #x6b4bb9af #xc4bfe81b #x66282193
     #x61d809cc #xfb21a991 #x487cac60 #x5dec8032 #xef845d5d #xe98575b1
     #xdc262302 #xeb651b88 #x23893e81 #xd396acc5 #x0f6d6ff3 #x83f44239
     #x2e0b4482 #xa4842004 #x69c8f04a #x9e1f9b5e #x21c66842 #xf6e96c9a
     #x670c9c61 #xabd388f0 #x6a51a0d2 #xd8542f68 #x960fa728 #xab5133a3
     #x6eef0b6c #x137a3be4 #xba3bf050 #x7efb2a98 #xa1f1651d #x39af0176
     #x66ca593e #x82430e88 #x8cee8619 #x456f9fb4 #x7d84a5c3 #x3b8b5ebe
     #xe06f75d8 #x85c12073 #x401a449f #x56c16aa6 #x4ed3aa62 #x363f7706
     #x1bfedf72 #x429b023d #x37d0d724 #xd00a1248 #xdb0fead3 #x49f1c09b
     #x075372c9 #x80991b7b #x25d479d8 #xf6e8def7 #xe3fe501a #xb6794c3b
     #x976ce0bd #x04c006ba #xc1a94fb6 #x409f60c4 #x5e5c9ec2 #x196a2463
     #x68fb6faf #x3e6c53b5 #x1339b2eb #x3b52ec6f #x6dfc511f #x9b30952c
     #xcc814544 #xaf5ebd09 #xbee3d004 #xde334afd #x660f2807 #x192e4bb3
     #xc0cba857 #x45c8740f #xd20b5f39 #xb9d3fbdb #x5579c0bd #x1a60320a
     #xd6a100c6 #x402c7279 #x679f25fe #xfb1fa3cc #x8ea5e9f8 #xdb3222f8
     #x3c7516df #xfd616b15 #x2f501ec8 #xad0552ab #x323db5fa #xfd238760
     #x53317b48 #x3e00df82 #x9e5c57bb #xca6f8ca0 #x1a87562e #xdf1769db
     #xd542a8f6 #x287effc3 #xac6732c6 #x8c4f5573 #x695b27b0 #xbbca58c8
     #xe1ffa35d #xb8f011a0 #x10fa3d98 #xfd2183b8 #x4afcb56c #x2dd1d35b
     #x9a53e479 #xb6f84565 #xd28e49bc #x4bfb9790 #xe1ddf2da #xa4cb7e33
     #x62fb1341 #xcee4c6e8 #xef20cada #x36774c01 #xd07e9efe #x2bf11fb4
     #x95dbda4d #xae909198 #xeaad8e71 #x6b93d5a0 #xd08ed1d0 #xafc725e0
     #x8e3c5b2f #x8e7594b7 #x8ff6e2fb #xf2122b64 #x8888b812 #x900df01c
     #x4fad5ea0 #x688fc31c #xd1cff191 #xb3a8c1ad #x2f2f2218 #xbe0e1777
     #xea752dfe #x8b021fa1 #xe5a0cc0f #xb56f74e8 #x18acf3d6 #xce89e299
     #xb4a84fe0 #xfd13e0b7 #x7cc43b81 #xd2ada8d9 #x165fa266 #x80957705
     #x93cc7314 #x211a1477 #xe6ad2065 #x77b5fa86 #xc75442f5 #xfb9d35cf
     #xebcdaf0c #x7b3e89a0 #xd6411bd3 #xae1e7e49 #x00250e2d #x2071b35e
     #x226800bb #x57b8e0af #x2464369b #xf009b91e #x5563911d #x59dfa6aa
     #x78c14389 #xd95a537f #x207d5ba2 #x02e5b9c5 #x83260376 #x6295cfa9
     #x11c81968 #x4e734a41 #xb3472dca #x7b14a94a #x1b510052 #x9a532915
     #xd60f573f #xbc9bc6e4 #x2b60a476 #x81e67400 #x08ba6fb5 #x571be91f
     #xf296ec6b #x2a0dd915 #xb6636521 #xe7b9f9b6 #xff34052e #xc5855664
     #x53b02d5d #xa99f8fa1 #x08ba4799 #x6e85076a)))

(defconstant* +initial-s-box-1+
  (make-blowfish-array
   '(#x4b7a70e9 #xb5b32944 #xdb75092e #xc4192623 #xad6ea6b0 #x49a7df7d
     #x9cee60b8 #x8fedb266 #xecaa8c71 #x699a17ff #x5664526c #xc2b19ee1
     #x193602a5 #x75094c29 #xa0591340 #xe4183a3e #x3f54989a #x5b429d65
     #x6b8fe4d6 #x99f73fd6 #xa1d29c07 #xefe830f5 #x4d2d38e6 #xf0255dc1
     #x4cdd2086 #x8470eb26 #x6382e9c6 #x021ecc5e #x09686b3f #x3ebaefc9
     #x3c971814 #x6b6a70a1 #x687f3584 #x52a0e286 #xb79c5305 #xaa500737
     #x3e07841c #x7fdeae5c #x8e7d44ec #x5716f2b8 #xb03ada37 #xf0500c0d
     #xf01c1f04 #x0200b3ff #xae0cf51a #x3cb574b2 #x25837a58 #xdc0921bd
     #xd19113f9 #x7ca92ff6 #x94324773 #x22f54701 #x3ae5e581 #x37c2dadc
     #xc8b57634 #x9af3dda7 #xa9446146 #x0fd0030e #xecc8c73e #xa4751e41
     #xe238cd99 #x3bea0e2f #x3280bba1 #x183eb331 #x4e548b38 #x4f6db908
     #x6f420d03 #xf60a04bf #x2cb81290 #x24977c79 #x5679b072 #xbcaf89af
     #xde9a771f #xd9930810 #xb38bae12 #xdccf3f2e #x5512721f #x2e6b7124
     #x501adde6 #x9f84cd87 #x7a584718 #x7408da17 #xbc9f9abc #xe94b7d8c
     #xec7aec3a #xdb851dfa #x63094366 #xc464c3d2 #xef1c1847 #x3215d908
     #xdd433b37 #x24c2ba16 #x12a14d43 #x2a65c451 #x50940002 #x133ae4dd
     #x71dff89e #x10314e55 #x81ac77d6 #x5f11199b #x043556f1 #xd7a3c76b
     #x3c11183b #x5924a509 #xf28fe6ed #x97f1fbfa #x9ebabf2c #x1e153c6e
     #x86e34570 #xeae96fb1 #x860e5e0a #x5a3e2ab3 #x771fe71c #x4e3d06fa
     #x2965dcb9 #x99e71d0f #x803e89d6 #x5266c825 #x2e4cc978 #x9c10b36a
     #xc6150eba #x94e2ea78 #xa5fc3c53 #x1e0a2df4 #xf2f74ea7 #x361d2b3d
     #x1939260f #x19c27960 #x5223a708 #xf71312b6 #xebadfe6e #xeac31f66
     #xe3bc4595 #xa67bc883 #xb17f37d1 #x018cff28 #xc332ddef #xbe6c5aa5
     #x65582185 #x68ab9802 #xeecea50f #xdb2f953b #x2aef7dad #x5b6e2f84
     #x1521b628 #x29076170 #xecdd4775 #x619f1510 #x13cca830 #xeb61bd96
     #x0334fe1e #xaa0363cf #xb5735c90 #x4c70a239 #xd59e9e0b #xcbaade14
     #xeecc86bc #x60622ca7 #x9cab5cab #xb2f3846e #x648b1eaf #x19bdf0ca
     #xa02369b9 #x655abb50 #x40685a32 #x3c2ab4b3 #x319ee9d5 #xc021b8f7
     #x9b540b19 #x875fa099 #x95f7997e #x623d7da8 #xf837889a #x97e32d77
     #x11ed935f #x16681281 #x0e358829 #xc7e61fd6 #x96dedfa1 #x7858ba99
     #x57f584a5 #x1b227263 #x9b83c3ff #x1ac24696 #xcdb30aeb #x532e3054
     #x8fd948e4 #x6dbc3128 #x58ebf2ef #x34c6ffea #xfe28ed61 #xee7c3c73
     #x5d4a14d9 #xe864b7e3 #x42105d14 #x203e13e0 #x45eee2b6 #xa3aaabea
     #xdb6c4f15 #xfacb4fd0 #xc742f442 #xef6abbb5 #x654f3b1d #x41cd2105
     #xd81e799e #x86854dc7 #xe44b476a #x3d816250 #xcf62a1f2 #x5b8d2646
     #xfc8883a0 #xc1c7b6a3 #x7f1524c3 #x69cb7492 #x47848a0b #x5692b285
     #x095bbf00 #xad19489d #x1462b174 #x23820e00 #x58428d2a #x0c55f5ea
     #x1dadf43e #x233f7061 #x3372f092 #x8d937e41 #xd65fecf1 #x6c223bdb
     #x7cde3759 #xcbee7460 #x4085f2a7 #xce77326e #xa6078084 #x19f8509e
     #xe8efd855 #x61d99735 #xa969a7aa #xc50c06c2 #x5a04abfc #x800bcadc
     #x9e447a2e #xc3453484 #xfdd56705 #x0e1e9ec9 #xdb73dbd3 #x105588cd
     #x675fda79 #xe3674340 #xc5c43465 #x713e38d8 #x3d28f89e #xf16dff20
     #x153e21e7 #x8fb03d4a #xe6e39f2b #xdb83adf7)))

(defconstant* +initial-s-box-2+
  (make-blowfish-array
   '(#xe93d5a68 #x948140f7 #xf64c261c #x94692934 #x411520f7 #x7602d4f7
     #xbcf46b2e #xd4a20068 #xd4082471 #x3320f46a #x43b7d4b7 #x500061af
     #x1e39f62e #x97244546 #x14214f74 #xbf8b8840 #x4d95fc1d #x96b591af
     #x70f4ddd3 #x66a02f45 #xbfbc09ec #x03bd9785 #x7fac6dd0 #x31cb8504
     #x96eb27b3 #x55fd3941 #xda2547e6 #xabca0a9a #x28507825 #x530429f4
     #x0a2c86da #xe9b66dfb #x68dc1462 #xd7486900 #x680ec0a4 #x27a18dee
     #x4f3ffea2 #xe887ad8c #xb58ce006 #x7af4d6b6 #xaace1e7c #xd3375fec
     #xce78a399 #x406b2a42 #x20fe9e35 #xd9f385b9 #xee39d7ab #x3b124e8b
     #x1dc9faf7 #x4b6d1856 #x26a36631 #xeae397b2 #x3a6efa74 #xdd5b4332
     #x6841e7f7 #xca7820fb #xfb0af54e #xd8feb397 #x454056ac #xba489527
     #x55533a3a #x20838d87 #xfe6ba9b7 #xd096954b #x55a867bc #xa1159a58
     #xcca92963 #x99e1db33 #xa62a4a56 #x3f3125f9 #x5ef47e1c #x9029317c
     #xfdf8e802 #x04272f70 #x80bb155c #x05282ce3 #x95c11548 #xe4c66d22
     #x48c1133f #xc70f86dc #x07f9c9ee #x41041f0f #x404779a4 #x5d886e17
     #x325f51eb #xd59bc0d1 #xf2bcc18f #x41113564 #x257b7834 #x602a9c60
     #xdff8e8a3 #x1f636c1b #x0e12b4c2 #x02e1329e #xaf664fd1 #xcad18115
     #x6b2395e0 #x333e92e1 #x3b240b62 #xeebeb922 #x85b2a20e #xe6ba0d99
     #xde720c8c #x2da2f728 #xd0127845 #x95b794fd #x647d0862 #xe7ccf5f0
     #x5449a36f #x877d48fa #xc39dfd27 #xf33e8d1e #x0a476341 #x992eff74
     #x3a6f6eab #xf4f8fd37 #xa812dc60 #xa1ebddf8 #x991be14c #xdb6e6b0d
     #xc67b5510 #x6d672c37 #x2765d43b #xdcd0e804 #xf1290dc7 #xcc00ffa3
     #xb5390f92 #x690fed0b #x667b9ffb #xcedb7d9c #xa091cf0b #xd9155ea3
     #xbb132f88 #x515bad24 #x7b9479bf #x763bd6eb #x37392eb3 #xcc115979
     #x8026e297 #xf42e312d #x6842ada7 #xc66a2b3b #x12754ccc #x782ef11c
     #x6a124237 #xb79251e7 #x06a1bbe6 #x4bfb6350 #x1a6b1018 #x11caedfa
     #x3d25bdd8 #xe2e1c3c9 #x44421659 #x0a121386 #xd90cec6e #xd5abea2a
     #x64af674e #xda86a85f #xbebfe988 #x64e4c3fe #x9dbc8057 #xf0f7c086
     #x60787bf8 #x6003604d #xd1fd8346 #xf6381fb0 #x7745ae04 #xd736fccc
     #x83426b33 #xf01eab71 #xb0804187 #x3c005e5f #x77a057be #xbde8ae24
     #x55464299 #xbf582e61 #x4e58f48f #xf2ddfda2 #xf474ef38 #x8789bdc2
     #x5366f9c3 #xc8b38e74 #xb475f255 #x46fcd9b9 #x7aeb2661 #x8b1ddf84
     #x846a0e79 #x915f95e2 #x466e598e #x20b45770 #x8cd55591 #xc902de4c
     #xb90bace1 #xbb8205d0 #x11a86248 #x7574a99e #xb77f19b6 #xe0a9dc09
     #x662d09a1 #xc4324633 #xe85a1f02 #x09f0be8c #x4a99a025 #x1d6efe10
     #x1ab93d1d #x0ba5a4df #xa186f20f #x2868f169 #xdcb7da83 #x573906fe
     #xa1e2ce9b #x4fcd7f52 #x50115e01 #xa70683fa #xa002b5c4 #x0de6d027
     #x9af88c27 #x773f8641 #xc3604c06 #x61a806b5 #xf0177a28 #xc0f586e0
     #x006058aa #x30dc7d62 #x11e69ed7 #x2338ea63 #x53c2dd94 #xc2c21634
     #xbbcbee56 #x90bcb6de #xebfc7da1 #xce591d76 #x6f05e409 #x4b7c0188
     #x39720a3d #x7c927c24 #x86e3725f #x724d9db9 #x1ac15bb4 #xd39eb8fc
     #xed545578 #x08fca5b5 #xd83d7cd3 #x4dad0fc4 #x1e50ef5e #xb161e6f8
     #xa28514d9 #x6c51133c #x6fd5c7e7 #x56e14ec4 #x362abfce #xddc6c837
     #xd79a3234 #x92638212 #x670efa8e #x406000e0)))

(defconstant* +initial-s-box-3+
  (make-blowfish-array
   '(#x3a39ce37 #xd3faf5cf #xabc27737 #x5ac52d1b #x5cb0679e #x4fa33742
     #xd3822740 #x99bc9bbe #xd5118e9d #xbf0f7315 #xd62d1c7e #xc700c47b
     #xb78c1b6b #x21a19045 #xb26eb1be #x6a366eb4 #x5748ab2f #xbc946e79
     #xc6a376d2 #x6549c2c8 #x530ff8ee #x468dde7d #xd5730a1d #x4cd04dc6
     #x2939bbdb #xa9ba4650 #xac9526e8 #xbe5ee304 #xa1fad5f0 #x6a2d519a
     #x63ef8ce2 #x9a86ee22 #xc089c2b8 #x43242ef6 #xa51e03aa #x9cf2d0a4
     #x83c061ba #x9be96a4d #x8fe51550 #xba645bd6 #x2826a2f9 #xa73a3ae1
     #x4ba99586 #xef5562e9 #xc72fefd3 #xf752f7da #x3f046f69 #x77fa0a59
     #x80e4a915 #x87b08601 #x9b09e6ad #x3b3ee593 #xe990fd5a #x9e34d797
     #x2cf0b7d9 #x022b8b51 #x96d5ac3a #x017da67d #xd1cf3ed6 #x7c7d2d28
     #x1f9f25cf #xadf2b89b #x5ad6b472 #x5a88f54c #xe029ac71 #xe019a5e6
     #x47b0acfd #xed93fa9b #xe8d3c48d #x283b57cc #xf8d56629 #x79132e28
     #x785f0191 #xed756055 #xf7960e44 #xe3d35e8c #x15056dd4 #x88f46dba
     #x03a16125 #x0564f0bd #xc3eb9e15 #x3c9057a2 #x97271aec #xa93a072a
     #x1b3f6d9b #x1e6321f5 #xf59c66fb #x26dcf319 #x7533d928 #xb155fdf5
     #x03563482 #x8aba3cbb #x28517711 #xc20ad9f8 #xabcc5167 #xccad925f
     #x4de81751 #x3830dc8e #x379d5862 #x9320f991 #xea7a90c2 #xfb3e7bce
     #x5121ce64 #x774fbe32 #xa8b6e37e #xc3293d46 #x48de5369 #x6413e680
     #xa2ae0810 #xdd6db224 #x69852dfd #x09072166 #xb39a460a #x6445c0dd
     #x586cdecf #x1c20c8ae #x5bbef7dd #x1b588d40 #xccd2017f #x6bb4e3bb
     #xdda26a7e #x3a59ff45 #x3e350a44 #xbcb4cdd5 #x72eacea8 #xfa6484bb
     #x8d6612ae #xbf3c6f47 #xd29be463 #x542f5d9e #xaec2771b #xf64e6370
     #x740e0d8d #xe75b1357 #xf8721671 #xaf537d5d #x4040cb08 #x4eb4e2cc
     #x34d2466a #x0115af84 #xe1b00428 #x95983a1d #x06b89fb4 #xce6ea048
     #x6f3f3b82 #x3520ab82 #x011a1d4b #x277227f8 #x611560b1 #xe7933fdc
     #xbb3a792b #x344525bd #xa08839e1 #x51ce794b #x2f32c9b7 #xa01fbac9
     #xe01cc87e #xbcc7d1f6 #xcf0111c3 #xa1e8aac7 #x1a908749 #xd44fbd9a
     #xd0dadecb #xd50ada38 #x0339c32a #xc6913667 #x8df9317c #xe0b12b4f
     #xf79e59b7 #x43f5bb3a #xf2d519ff #x27d9459c #xbf97222c #x15e6fc2a
     #x0f91fc71 #x9b941525 #xfae59361 #xceb69ceb #xc2a86459 #x12baa8d1
     #xb6c1075e #xe3056a0c #x10d25065 #xcb03a442 #xe0ec6e0e #x1698db3b
     #x4c98a0be #x3278e964 #x9f1f9532 #xe0d392df #xd3a0342b #x8971f21e
     #x1b0a7441 #x4ba3348c #xc5be7120 #xc37632d8 #xdf359f8d #x9b992f2e
     #xe60b6f47 #x0fe3f11d #xe54cda54 #x1edad891 #xce6279cf #xcd3e7e6f
     #x1618b166 #xfd2c1d05 #x848fd2c5 #xf6fb2299 #xf523f357 #xa6327623
     #x93a83531 #x56cccd02 #xacf08162 #x5a75ebb5 #x6e163697 #x88d273cc
     #xde966292 #x81b949d0 #x4c50901b #x71c65614 #xe6c6c7bd #x327a140a
     #x45e1d006 #xc3f27b9a #xc9aa53fd #x62a80f00 #xbb25bfe2 #x35bdd2f6
     #x71126905 #xb2040222 #xb6cbcf7c #xcd769c2b #x53113ec0 #x1640e3d3
     #x38abbd60 #x2547adf0 #xba38209c #xf746ce76 #x77afa1c5 #x20756060
     #x85cbfe4e #x8ae88dd8 #x7aaaf9b0 #x4cf9aa7e #x1948c25c #x02fb8a8c
     #x01c36ae4 #xd6ebe1f9 #x90d4f869 #xa65cdea0 #x3f09252d #xc208e69f
     #xb74e6132 #xce77e25b #x578fdfe3 #x3ac372e6)))

;;;; Blowfish buffers/keys

(defstruct (blowfish-buffer (:conc-name buffer-))
  (left  0 :type unsigned-byte-32)
  (right 0 :type unsigned-byte-32))


(defstruct (blowfish-key (:constructor %-make-blowfish-key))
  (p-array (copy-seq +initial-p-array+))
  (s-boxes (vector (copy-seq +initial-s-box-0+)
		   (copy-seq +initial-s-box-1+)
		   (copy-seq +initial-s-box-2+)
		   (copy-seq +initial-s-box-3+))))

;;;; Public API functions

;; Optimization hints
;;
;(declaim (inline make-blowfish-buffer
;                 buffer-left  (setf buffer-left)
;                 buffer-right (setf buffer-right)))

;; Design notes
;; ------------
;; blowfish works by scrambling the data in two 32-bit numbers
;; with the p-array and s-boxes.  These two 32-bit numbers
;; are held in memory in a BLOWFISH-BUFFER, and modified
;; in place.
;;
;; blowfish is a variable length key cipher.  The key, in
;; plaintext, is converted to a sequence of 32-bit numbers,
;; which are munged repeatedly into a blowfish-key, which is
;; just a specific p-array and s-boxes.  (The original p-array
;; and s-boxes are populated from the digits of PI, for the curious).
;; In normal usage, one would create a blowfish key once (which is
;; very expensive) and repeatedly use it as an argument to the
;; other encryption/decryption functions.
;;
;; We introduce the concept of a key designator:
;; Key Designator: a blowfish key, or an object from which a blowfish key
;;                 can be constructed.  At present, such objects are:
;;                 non-null string
;;                 a vector of (preferably 32 bit long) numbers
;;                 a 64bit long number (TO BE FIXMED)
;;



(declaim (inline blowfish-F))
(defun blowfish-F (num s-boxes)
  "The blowfish F function.

  F(xL) = ( (S1,a + S2,b mod 2^32) XOR S3,c) + S4,d mod 2^32
  Where a,b,c,d are the 4 bytes of num. and Si are
  the 4 entries of the vector of s-boxes."
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (declare (type unsigned-byte-32 num))
  (declare (type (simple-vector 4) s-boxes))

  ;; Grab each byte of 32-bit num
  (with-numeric-fields ((a b c d) num :size 8)
    (declare (type unsigned-byte a b c d))
    (macrolet ((lookup-sbox (box index)
		`(aref (the blowfish-array (svref s-boxes ,box))
		       (the fixnum ,index))))
      (the unsigned-byte-32
	(mod32-add (lookup-sbox 3 d)
		   (fast-32bit-xor (lookup-sbox 2 c)
				   (mod32-add (lookup-sbox 1 b)
					      (lookup-sbox 0 a))))))))

(macrolet ((pair-xorf (left right i)
	     `(progn (xorf ,left (the unsigned-byte-32 (aref p-array ,i)))
	       (xorf ,right (blowfish-f ,left s-boxes))))
	   (forward-round ()
	     `(progn
	       ,@(loop for i from 0 upto 15
		       for left = 'left then right
		       and right = 'right then left
		       collect `(pair-xorf ,left ,right ,i)))))
  (defun blowfish-forward-round (buf p-array s-boxes)
    "Encrypt BUF through 1 round of the blowfish algorithm,
   using the provided p-array and s-boxes."
    (declare (optimize (safety 0) (debug 0) (speed 3)))
    (declare (type blowfish-array p-array))
    (declare (type blowfish-buffer buf))
    (let ((left (buffer-left buf))
	  (right (buffer-right buf)))
      (forward-round)
      (rotatef left right)

      ;; use the last two p-array elements
      (setf (buffer-right buf)
	    (xorf right (the unsigned-byte-32 (aref p-array 16))))
      (setf (buffer-left buf)
	    (xorf left (the unsigned-byte-32 (aref p-array 17))))
      t)))
(macrolet ((pair-xorf (left right i)
	     `(progn (xorf ,left (the unsigned-byte-32 (aref p-array ,i)))
	       (xorf ,right (blowfish-f ,left s-boxes))))
	   (reverse-round ()
	     `(progn
	       ,@(loop for i from 17 downto 2
		       for left = 'left then right
		       and right = 'right then left
		       collect `(pair-xorf ,left ,right ,i)))))
  (defun blowfish-reverse-round (buf p-array s-boxes)
    "Decrypt BUF through 1 round of the blowfish algorithm,
   using the provided p-array and s-boxes."
    (declare (optimize (safety 0) (debug 0) (speed 3)))
    (declare (type blowfish-array p-array))
    (declare (type blowfish-buffer buf))
    (let ((left (buffer-left buf))
	  (right (buffer-right buf)))
      (reverse-round)

      (rotatef left right)

      ;; Use the last two p-array elements
      (setf (buffer-right buf)
	    (xorf right (the unsigned-byte-32 (aref p-array 1))))
      (setf (buffer-left buf)
	    (xorf left (the unsigned-byte-32 (aref p-array 0))))
      t)))

(defun blowfish-round (buf p-array s-boxes reverse?)
  "Encrypt BUF through 1 round of the blowfish algorithm,
   using the provided p-array and s-boxes.
   If REVERSE? is T, perform decryption instead."
  (if reverse?
      (blowfish-forward-round buf p-array s-boxes)
      (blowfish-reverse-round buf p-array s-boxes)))

(declaim (inline blowfish-encrypt))
(defun blowfish-encrypt (blowfish-buf blowfish-key)
  "Encrypt the contents of BLOWFISH-BUF using BLOWFISH-KEY.
   BLOWFISH-BUF must have been obtained by MAKE-BLOWFISH-BUFFER.
   BLOWFISH-KEY must have been obtained by MAKE-BLOWFISH-KEY."
  (blowfish-forward-round blowfish-buf
		  (blowfish-key-p-array blowfish-key)
		  (blowfish-key-s-boxes blowfish-key)))


(declaim (inline blowfish-decrypt))
(defun blowfish-decrypt (blowfish-buf blowfish-key)
  "Decrypt the contents of BLOWFISH-BUF using BLOWFISH-KEY.
   BLOWFISH-BUF must have been obtained by MAKE-BLOWFISH-BUFFER.
   BLOWFISH-KEY must have been obtained by MAKE-BLOWFISH-KEY."
  (blowfish-reverse-round blowfish-buf
		  (blowfish-key-p-array blowfish-key)
		  (blowfish-key-s-boxes blowfish-key)
		  ))


;;; Mixing functions

(defun mix-vector! (vector buffer p-array s-boxes)
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (declare (type blowfish-array p-array vector)
	   (type (simple-vector 4) s-boxes)
	   (type blowfish-buffer buffer))
  (loop with length fixnum = (/ (length vector) 2)
	repeat (the fixnum length)
	for left fixnum upfrom 0 by 2
	for right fixnum upfrom 1 by 2
	do
	(blowfish-forward-round buffer p-array s-boxes)
	(setf (aref vector left)  (buffer-left buffer))
	(setf (aref vector right) (buffer-right buffer))))


(defun mix-p-array! (buffer p-array s-boxes)
  "Performs blowfish rounds on BUFFER,
   repeatedly modifiying the P-ARRAY the intermediate results."
  ;; This munges p-array, but not the s-boxes.
  (mix-vector! p-array buffer p-array s-boxes))

(defun mix-s-boxes! (buffer p-array s-boxes)
  "Munge the S-BOXES by repeatedly their contents with
   results of a blowfish-round on BUFFER."
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (declare (type (simple-vector 4) s-boxes)
	   (type blowfish-array    p-array))
  (loop for box across s-boxes
	do (mix-vector! box buffer p-array s-boxes)))

(defun xor-p-array-with-repeated-key! (p-array key-seq)
  "Munge the p-array by repeatedly XORing with KEY-SEQ.  Destructive."
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (declare (type blowfish-array p-array)
	   (simple-vector key-seq))
  (let ((len (length key-seq)))
    (assert (> len 0))
    (loop for i below 18 do
	  (xorf (aref p-array i) (svref key-seq (mod i len))))))

(defun initialize-blowfish-boxes (key-seq p-array s-boxes)
  "Return two values, p-array and s-boxes, suitable
   for use in a blowfish round.
   KEY must be a vector of numbers."
  ;; The first round of randomization just
  ;; does XOS on initial p-array with the key.
  (xor-p-array-with-repeated-key! p-array key-seq)
  (let ((buffer (make-blowfish-buffer)))
    ;; The next round repeatedly cycle through the p-array
    ;; and s-boxes, replacing their contents by
    ;; blowfish rounds on the same buffer, which keeps
    ;; being recycled.
    (mix-p-array! buffer p-array s-boxes)
    (mix-s-boxes! buffer p-array s-boxes)))

;; Public
(defun make-blowfish-key (seed)
  "Use SEED to return a BLOWFISH-KEY suitable for use
   in BLOWFISH-ENCRYPT and BLOWFISH-DECRYPT."
  ;; This function creates a blowfish-key struct,
  ;; initializing the p-array and boxes according to
  ;; the blowfish initialization sequence from SEED.

  ;; Canonicalize the seed into a vector of 32bit numbers.
  (etypecase seed
    (string     (setf seed (string->key-vec seed)))
    (vector     (assert (every #'numberp seed)
			nil
			"Valid seed must be a vector of numbers"))
    (number     (setf seed (coerce (split-number seed 32) 'vector))))

  (let ((key (%-make-blowfish-key)))
    (initialize-blowfish-boxes seed
			       (blowfish-key-p-array key)
			       (blowfish-key-s-boxes key))
    key))

;;; FIXME: Hum... turn this into a generic ECB streaming function...
(defun blowfish-encrypt-string (string key)
  "Return a string containing STRING encrypted with KEY.
   KEY must be a valid key designator."
  (unless (blowfish-key-p key)
    (setf key (make-blowfish-key key)))

  (let ((buf (make-blowfish-buffer))
	(vec (string->key-vec string)))
    (loop repeat (ceiling (length vec) 2)
	  for i upfrom 0
	  for j upfrom 1
	  do
	  (setf (buffer-left buf)  (svref vec i))
	  (buffer-right buf) (svref vec j)
	  (blowfish-encrypt buf key)
	  collect (buffer-left buf) into output
	  collect (buffer-right buf) into output
	  finally (return (coerce output 'vector)))))


;;;; Testing

(defun test-initial-blowfish-round (buf &optional reverse?)
  "This lets you try a blowfish round using the initial
   p-array and S boxes, without blowing them away."
  (let ((p-array (copy-seq +initial-p-array+))
	(s-boxes (vector   (copy-seq +initial-s-box-0+)
			   (copy-seq +initial-s-box-1+)
			   (copy-seq +initial-s-box-2+)
			   (copy-seq +initial-s-box-3+))))
    (blowfish-round buf p-array s-boxes reverse?)))


(defparameter *test-data*
  '(
    ;; key bytes               clear bytes            cipher bytes
    (#x0000000000000000 #x0000000000000000 #x4EF997456198DD78)
    (#xFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF #x51866FD5B85ECB8A)
    (#x3000000000000000 #x1000000000000001 #x7D856F9A613063F2)
    (#x1111111111111111 #x1111111111111111 #x2466DD878B963C9D)
    (#x0123456789ABCDEF #x1111111111111111 #x61F9C3802281B096)
    (#x1111111111111111 #x0123456789ABCDEF #x7D0CC630AFDA1EC7)
    (#x0000000000000000 #x0000000000000000 #x4EF997456198DD78)
    (#xFEDCBA9876543210 #x0123456789ABCDEF #x0ACEAB0FC6A0A28D)
    (#x7CA110454A1A6E57 #x01A1D6D039776742 #x59C68245EB05282B)
    (#x0131D9619DC1376E #x5CD54CA83DEF57DA #xB1B8CC0B250F09A0)
    (#x07A1133E4A0B2686 #x0248D43806F67172 #x1730E5778BEA1DA4)
    (#x3849674C2602319E #x51454B582DDF440A #xA25E7856CF2651EB)
    (#x04B915BA43FEB5B6 #x42FD443059577FA2 #x353882B109CE8F1A)
    (#x0113B970FD34F2CE #x059B5E0851CF143A #x48F4D0884C379918)
    (#x0170F175468FB5E6 #x0756D8E0774761D2 #x432193B78951FC98)
    (#x43297FAD38E373FE #x762514B829BF486A #x13F04154D69D1AE5)
    (#x07A7137045DA2A16 #x3BDD119049372802 #x2EEDDA93FFD39C79)
    (#x04689104C2FD3B2F #x26955F6835AF609A #xD887E0393C2DA6E3)
    (#x37D06BB516CB7546 #x164D5E404F275232 #x5F99D04F5B163969)
    (#x1F08260D1AC2465E #x6B056E18759F5CCA #x4A057A3B24D3977B)
    (#x584023641ABA6176 #x004BD6EF09176062 #x452031C1E4FADA8E)
    (#x025816164629B007 #x480D39006EE762F2 #x7555AE39F59B87BD)
    (#x49793EBC79B3258F #x437540C8698F3CFA #x53C55F9CB49FC019)
    (#x4FB05E1515AB73A7 #x072D43A077075292 #x7A8E7BFA937E89A3)
    (#x49E95D6D4CA229BF #x02FE55778117F12A #xCF9C5D7A4986ADB5)
    (#x018310DC409B26D6 #x1D9D5C5018F728C2 #xD1ABB290658BC778)
    (#x1C587F1C13924FEF #x305532286D6F295A #x55CB3774D13EF201)
    (#x0101010101010101 #x0123456789ABCDEF #xFA34EC4847B268B2)
    (#x1F1F1F1F0E0E0E0E #x0123456789ABCDEF #xA790795108EA3CAE)
    (#xE0FEE0FEF1FEF1FE #x0123456789ABCDEF #xC39E072D9FAC631D)
    (#x0000000000000000 #xFFFFFFFFFFFFFFFF #x014933E0CDAFF6E4)
    (#xFFFFFFFFFFFFFFFF #x0000000000000000 #xF21E9A77B71C49BC)
    (#x0123456789ABCDEF #x0000000000000000 #x245946885754369A)
    (#xFEDCBA9876543210 #xFFFFFFFFFFFFFFFF #x6B5C5A9C5D9E0A5A))
  "Test vector obtained from counterpane.com.")

(defun number->blowfish-buffer (number)
  "Return a vector containing two 32 bit numbers:
   the left 2 bytes and the right 2 bytes."
  (make-blowfish-buffer :left  (ash (logand #xFFFFFFFF00000000 number) -32)
			:right (logand #xFFFFFFFF number)))



(defun test-vector (key clear cipher)
  (let* ((blowfish-key (make-blowfish-key (split-64-bit key)))
	 (clear-vals (split-64-bit clear))
	 (buf (make-blowfish-buffer :left  (svref clear-vals 0)
				    :right (svref clear-vals 1))))

      (blowfish-encrypt buf blowfish-key)
      (assert (= cipher (join-32-bit (buffer-left buf)
				     (buffer-right buf)))
	      nil
	      "Test fails encryption on ~A ~A ~A" key clear cipher)

      ;; Decryption
      (blowfish-decrypt buf blowfish-key)
      (assert (= clear (join-32-bit (buffer-left buf)
				    (buffer-right buf)))
	      nil
	      "Test fails decryption on ~A ~A ~A" key clear cipher)

      'OK))

(defun run-test-harness ()
  (loop for (key clear cipher) in *test-data* do
	(test-vector key clear cipher))
  'OK)

(defun test-speed (&key (key (random (expt 2 64)))
		        (clear (random (expt 2 64)))
		        (ntimes 1000000))
  (let* ((blowfish-key (make-blowfish-key (split-64-bit key)))
	 (clear-vals (split-64-bit clear))
	 (buf (make-blowfish-buffer :left  (svref clear-vals 0)
				    :right (svref clear-vals 1))))
    (dotimes (i ntimes)
      (declare (fixnum i))
      (blowfish-encrypt buf blowfish-key))))

;;;; blowfish.lisp ends here

(defmacro with-buffer-chunks ((left right blowfish-buffer) &body body)
  "Execute BODY in context where LEFT, RIGHT are the
   SETF-able left and right parts of BLOWFISH-BUFFER."
  `(symbol-macrolet ((,left  (buffer-left ,blowfish-buffer))
		     (,right (buffer-right ,blowfish-buffer)))
    ,@body))

(defun 32bit-list->string (vec)
  (let ((string (make-string (* 4 (length vec)))))
    (loop with pos = -1
	  for num in vec do
	  (with-numeric-fields ((a b c d) num :size 8)
	    (setf (aref string (incf pos)) (code-char a)
		  (aref string (incf pos)) (code-char b)
		  (aref string (incf pos)) (code-char c)
		  (aref string (incf pos)) (code-char d))))
    string))

(defun blowfish-encrypt-string-cbc (string key)
  "Return a string containing STRING encrypted with KEY.
   KEY must be a valid key designator."
  (unless (blowfish-key-p key)
    (setf key (make-blowfish-key key)))

  (let ((buf (make-blowfish-buffer))
	(vec (string->key-vec string)))
    (with-buffer-chunks (xL xR buf)
      (with-buffer-chunks (p-left p-right previous-ciphertext)

	;; To encrypt in CBC mode, we must keep track of
	;; previous ciphertext block
	(loop repeat (ceiling (length vec) 2)
	      with previous-ciphertext = (make-blowfish-buffer
					  :left #xFFFFFFFF :right #xFFFFFFFF)
	      for i upfrom 0
	      for j upfrom 1
	      do

	      (setf xL (logxor (svref vec i) p-left)
		    xR (logxor (svref vec j) p-right))

	      (blowfish-encrypt buf key)

	      (setf p-left  xL
		    p-right xR)

	      collect xL into output
	      collect xR into output
	      finally (return (32bit-list->string output)))))))

(defun blowfish-decrypt-string-cbc (string key)
  "Return a string containing STRING encrypted with KEY.
   KEY must be a valid key designator."
  (unless (blowfish-key-p key)
    (setf key (make-blowfish-key key)))

  (let ((buf (make-blowfish-buffer))
	(vec (string->key-vec string)))

    ;; To decrypt in CBC mode, we must keep track of
    ;; previous ciphertext block
    (loop repeat (ceiling (length vec) 2)
	  with previous-ciphertext = (make-blowfish-buffer)
	  for i upfrom 2
	  for j upfrom 3

	  initially			; Set the previous ciphertext
	  (setf (buffer-left buf)  (svref vec 0)
		(buffer-right buf) (svref vec 1))
	  (blowfish-decrypt buf key)
	  (setf (buffer-left previous-ciphertext) (buffer-left buf)
		(buffer-right previous-ciphertext) (buffer-right buf))

	  do
	  (blowfish-decrypt buf key)
	  (setf (buffer-left previous-ciphertext) (buffer-left buf)
		(buffer-right previous-ciphertext) (buffer-right buf))

	  (setf (buffer-left buf)
		(logxor (svref vec i)
			(buffer-right previous-ciphertext))

		(buffer-right buf)
		(logxor (svref vec j)
			(buffer-left previous-ciphertext)))


	  collect (buffer-left buf) into output
	  collect (buffer-right buf) into output
	  finally (return (32bit-list->string output)))))

