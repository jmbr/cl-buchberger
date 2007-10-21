(in-package :com.superadditive.cl-buchberger)

;; Copyright (C) 2007 Juan M. Bello Rivas <jmbr@superadditive.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defclass term (ring-element)
  ((coefficient
    :initarg :coefficient
    :initform 0
    :accessor coefficient)
   (monomial
    :type vector
    :initarg :monomial
    :initform nil
    :accessor monomial)))

(defmethod initialize-instance :after ((tm term) &key ring)
  (when (null (monomial tm))
    (setf (monomial tm)
	  (make-array (length (variables ring))
		      :element-type 'integer :initial-element 0))))

(defmethod ring-zero-p ((tm term))
  (and (zerop (coefficient tm))
       (vector-zero-p (monomial tm))))

(defmethod print-object ((tm term) stream)
  (print-unreadable-object (tm stream :type t)
    (format stream "~a ~a" (coefficient tm) (monomial tm))))

(defmethod element->string ((tm term) &key ring leading-term)
  (when tm
    (with-output-to-string (s)
      (with-slots (coefficient monomial) tm
	(flet ((independent-term-p (monomial)
		 (every #'zerop monomial))
	       (print-variables ()
		 (dotimes (i (length monomial))
		   (let ((exponent (aref monomial i)))
		     (when (not (zerop exponent))
		       (format s "~a"
			       (string-downcase
				(elt (variables ring) i)))
		       (when (/= 1 exponent)
			 (format s "^~d" exponent)))))))
	  (if (plusp (signum coefficient))
	      (format s (if leading-term "" "+ "))
	      (format s (if leading-term "-" "- ")))
	  (if (independent-term-p monomial)
	      (format s "~d" (abs coefficient))
	      (progn
		(when (/= 1 (abs coefficient))
		  (format s "~d" (abs coefficient)))
		(print-variables))))))))

(defmethod ring-equal-p ((t1 term) (t2 term))
  (with-slots ((c1 coefficient) (m1 monomial)) t1
    (with-slots ((c2 coefficient) (m2 monomial)) t2
      (and (= c1 c2)
	   (equalp m1 m2)))))
