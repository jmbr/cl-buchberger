(in-package :com.superadditive.cl-buchberger)

;; Copyright (C) 2009  Juan M. Bello Rivas <jmbr@superadditive.com>
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

(defclass ideal ()
  ((ring :initarg :ring
         :initform (error "You must specify a ring for the polynomial ideal.")
         :accessor ring)
   (generators :initarg :generators
               :initform (error "You give a set of generators for the ideal.")
               :accessor generators)))

(defun make-ideal (generators)
  (when (and generators (consp generators))
    (let* ((g (first generators))
           (base-ring (base-ring g)))
      (unless (every (lambda (x) (eq x base-ring))
                     (mapcar #'base-ring (rest generators)))
        (error "Every generator must belong to the same ring."))
      (make-instance 'ideal
                     :ring base-ring
                     :generators (make-array (length generators)
                                             :element-type (class-of g)
                                             :initial-contents generators)))))

(defmethod print-object ((ideal ideal) stream)
  (print-unreadable-object (ideal stream :type t)
    (format stream ":RING ~a :GENERATORS ~a"
            (slot-value ideal 'ring)
            (slot-value ideal 'generators))))
