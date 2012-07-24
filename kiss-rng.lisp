;;;; kiss-rng.lisp

(in-package #:kiss-rng)

(declaim (optimize (speed 3) (safety 0)))

(defstruct (superkiss-state (:conc-name sks-))
  (Q (make-array '(41790) :element-type '(unsigned-byte 32))
     :type (simple-array (unsigned-byte 32) (41790)))
  (indx 41790 :type (integer 0 41790))
  (carry 362436 :type (unsigned-byte 32))
  (xcng 1236789 :type (unsigned-byte 32))
  (xs 521288629 :type (unsigned-byte 32)))

(defstruct (superkiss64-state (:conc-name sks64-))
  (Q (make-array '(20632) :element-type '(unsigned-byte 64))
     :type (simple-array (unsigned-byte 64) (41790)))
  (indx 20632 :type (integer 0 20632))
  (carry 36243678541 :type (unsigned-byte 64))
  (xcng 12367890123456 :type (unsigned-byte 64))
  (xs 521288629546311 :type (unsigned-byte 64)))

(declaim (inline 32+ 32* 32>> 32<< 32^ 32~ 64+ 64* 64<< 64>> 64~ 64^ 64-upper 64-lower))
(declaim (inline cng xs supr kiss))
(declaim (inline cng64 xs64 supr64 kiss64))
(declaim (ftype (function (superkiss64-state) (unsigned-byte 64)) refill64))

(defun 32* (x y)
  (declare (type (unsigned-byte 32) x y))
  (logand (* x y) #xffffffff))

(defun 64* (x y)
  (declare (type (unsigned-byte 64) x y))
  (logand (* x y) #xffffffffffffffff))

(defun 32+ (x y)
  (declare (type (unsigned-byte 32) x y))
  (logand (+ x y) #xffffffff))

(defun 64+ (x y)
  (declare (type (unsigned-byte 64) x y))
  (logand (+ x y) #xffffffffffffffff))

(defun 32<< (x y)
  (declare (type (unsigned-byte 32) x)
	   (type (integer 0 32) y))
  (logand (ash x y) #xffffffff))

(defun 64<< (x y)
  (declare (type (unsigned-byte 64) x)
	   (type (integer 0 63) y))
  (logand (ash x y) #xffffffffffffffff))

(defun 32>> (x y)
  (declare (type (unsigned-byte 32) x)
           (type (integer 0 32) y))
  (logand (ash x (- y)) #xffffffff))

(defun 64>> (x y)
  (declare (type (unsigned-byte 64) x)
           (type (integer 0 64) y))
  (logand (ash x (- y)) #xffffffffffffffff))

(defun 32^ (x y)
  (declare (type (unsigned-byte 32) x y))
  (logand (logxor x y) #xffffffff))

(defun 64^ (x y)
  (declare (type (unsigned-byte 64) x y))
  (logand (logxor x y) #xffffffffffffffff))

(defun 32~ (x)
  (declare (type (unsigned-byte 32) x))
  (logand (lognot x) #xffffffff))

(defun 64~ (x)
  (declare (type (unsigned-byte 64) x))
  (logand (lognot x) #xffffffffffffffff))

(defun 64-upper (x)
  (declare (type (unsigned-byte 64) x))
  (ash x -32))

(defun 64-lower (x)
  (declare (type (unsigned-byte 64) x))
  (logand x #xffffffff))

(defun cng (state)
  (declare (type superkiss-state state))
  (setf (sks-xcng state) (32+ (32* (sks-xcng state) 69609) 123)))

(defun cng64 (state)
  (declare (type superkiss64-state state))
  (setf (sks64-xcng state) (64+ (64* (sks64-xcng state) 6906969069) 123)))

(defun xs (state)
  (declare (type superkiss-state state))
  (let ((xs (sks-xs state)))
    (setq xs (32^ xs (32<< xs 13)))
    (setq xs (32^ xs (32>> xs 17)))
    (setf (sks-xs state) (32^ xs (32>> xs 5)))))

(defun xs64 (state)
  (declare (type superkiss64-state state))
  (let ((xs (sks64-xs state)))
    (declare (type (unsigned-byte 64) xs))
    (setq xs (64^ xs (64<< xs 13)))
    (setq xs (64^ xs (64>> xs 17)))
    (setf (sks64-xs state) (64^ xs (64<< xs 43)))))

(defun refill (state)
  (declare (type superkiss-state state))
    (dotimes (i 41790)
      (let  ((tmp (64+ (64* 7010176 (aref (sks-Q state) i))
		 (sks-carry state))))
	(declare (type (unsigned-byte 64) tmp))
	(setf (sks-carry state) (64-upper tmp))
	(setf (aref (sks-Q state) i) (32~ (64-lower tmp)))))
    (setf (sks-indx state) 1)
    (aref (sks-Q state) 0))

(defun refill64 (state)
  (declare (type superkiss64-state state))
    (dotimes (i 20632)
      (let  ((h (logand (sks64-carry state) 1))
	     (z (64+
		 (64+
		  (logand (ash (aref (sks64-Q state) i) 40) #x7fffffffffffffff)
		  (logand (ash (aref (sks64-Q state) i) 38) #x7fffffffffffffff))
		 (64>> (sks64-carry state) 1))))
	(setf (sks64-carry state)
	      (64+
	       (64+
		(64>> (aref (sks64-Q state) i) 23)
		(64>> (aref (sks64-Q state) i) 25))
	       (64>> z 63)))
	(setf (aref (sks64-Q state) i)
	      (64~ (64+ (64<< z 1) h)))))
	(setf (sks64-indx state) 1)
	(aref (sks64-Q state) 0))
		     
		   

(defun supr (state)
  (declare (type superkiss-state state))
  (if (< (sks-indx state) 41790)
      (prog1
	  (aref (sks-Q state) (sks-indx state))
	(incf (sks-indx state)))
      (refill state)))

(defun supr64 (state)
  (declare (type superkiss64-state state))
  (if (< (sks64-indx state) 20632)
      (prog1
	  (aref (sks64-Q state) (sks64-indx state))
	(incf (sks64-indx state)))
      (refill64 state)))

(defun kiss (state)
  (declare (type superkiss-state state))
  (32+ (32+ (supr state) (cng state)) (xs state)))

(defun kiss64 (state)
  (declare (type superkiss64-state state))
  (64+ (64+ (supr64 state) (cng64 state)) (xs64 state)))

(defun sks-init (state)
  (declare (type superkiss-state state))
  (dotimes (i 41790)
    (setf (aref (sks-Q state) i) (32+ (cng state) (xs state)))))

(defun sks64-init (state)
  (declare (type superkiss64-state state))
  (dotimes (i 20632)
    (setf (aref (sks64-Q state) i) (64+ (cng64 state) (xs64 state)))))

(defun test (&aux (state (make-superkiss-state)))
  (sks-init state)
  (dotimes (i 999999999) (kiss state))
  (assert (= 3422554850 (kiss state))))

(defun test64 (&aux (state (make-superkiss64-state)))
  (sks64-init state)
  (dotimes (i 999999999) (kiss64 state))
  (assert (= 4013566000157423768 (kiss64 state))))
