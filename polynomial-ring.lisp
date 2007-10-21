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

(defclass polynomial-ring (ring)
  ((variables
    :reader variables
    :initarg :variables
    :initform (error "You must specify at least one variable"))
   (base-field
    :reader base-field
    :initarg :base-field
    :initform 'rational)))

(defmethod initialize-instance :after ((ring polynomial-ring) &key)
  (flet ((make-variables ()
	   (let ((vars (variables ring)))
	     (if (consp vars)
		 vars
		 (list vars)))))
    (setf (slot-value ring 'variables) (make-variables))))

(defmethod print-object ((ring polynomial-ring) stream)
  (print-unreadable-object (ring stream :type t)
   (let ((symbols (mapcar #'symbol-name (variables ring))))
    (format stream "~{~a~^, ~} over ~a"
	     symbols (base-field ring)))))

(defvar *ring*
  (make-instance 'polynomial-ring :variables (list 'x 'y 'z))
  "Default polynomial ring")

(defmacro with-polynomial-ring (ring &body body)
  `(let ((*ring* ,ring))
     ,@body))
