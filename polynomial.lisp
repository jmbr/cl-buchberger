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

(defclass polynomial (ring-element)
  ((base-ring
    :initarg :ring
    :initform (error "You must specify a base ring")
    :accessor base-ring)
   (terms
    :type hash-table
    :initarg :terms
    :initform (make-hash-table :test #'equalp)
    :accessor terms)))

(defgeneric lt (poly)
  (:documentation "Returns the leading term of a polynomial."))

(defgeneric lm (poly)
  (:documentation "Returns the leading monomial of a polynomial.
That is, the leading term with 1 as coefficient"))

(defgeneric lc (poly)
  (:documentation "Returns the leading coefficient of a polynomial"))

(defgeneric multideg (poly)
  (:documentation "Returns the multidegree of a polynomial"))

(defun make-polynomial (term-list &key (ring *ring*))
  (let ((poly (make-instance 'polynomial :ring ring)))
    (dolist (elem term-list poly)
      (setf poly
            (add poly
                 (make-instance 'term
                                :coefficient (first elem)
                                :monomial (make-array (1- (length elem))
                                                      :initial-contents (rest elem))))))))

(defmacro doterms ((var poly &optional (resultform 'nil)) &body body)
  (let ((mono (gensym "DOTERMS"))
	(coef (gensym "DOTERMS")))
    `(block nil
       (loop for ,mono being the hash-keys of (terms ,poly) using (hash-value ,coef) do
	    (let ((,var (make-instance 'term :coefficient ,coef :monomial ,mono)))
	      ,@body))
       (return ,resultform))))

(defun mapterm (function polynomial)
  "Apply FUNCTION to successive terms of POLYNOMIAL.  Return list
of FUNCTION return values."
  (let ((results nil))
    (doterms (tt polynomial results)
      (push (funcall function tt) results))))

(defmethod print-object ((p polynomial) stream)
  (print-unreadable-object (p stream :type t)
    (write-string (element->string p) stream)))

(defun terms->list (poly)
  (mapterm #'identity poly))

(defmethod element->string ((poly polynomial) &key)
  (assert poly)
  (if (ring-zero-p poly)
      (format nil "0")
      (let ((term-list
	     (sort (terms->list poly) *monomial-ordering* :key #'monomial)))
	(format nil "~a~{ ~a~}"
		(element->string (first term-list)
				 :ring (base-ring poly)
				 :leading-term t)
		(mapcar #'(lambda (x)
			    (element->string x :ring (base-ring poly)))
			(rest term-list))))))

(defmethod ring-zero-p ((poly polynomial))
  (zerop (hash-table-count (terms poly))))

(defmethod ring-copy ((poly polynomial))
  "Returns a (deep) copy of the given polynomial"
  (let ((new-poly (make-instance 'polynomial :ring (base-ring poly))))
    (maphash #'(lambda (m c)
		 (setf (gethash (copy-seq m) (terms new-poly)) c))
	     (terms poly))
    new-poly))

(defmethod lt ((poly polynomial))
  (let ((term-list (terms->list poly)))
    (first (sort term-list *monomial-ordering* :key #'monomial))))

(defmethod lc ((poly polynomial))
  (coefficient (lt poly)))

(defmethod lm ((poly polynomial))
  (monomial (lt poly)))

(defmethod multideg ((poly polynomial))
  (lc poly))
