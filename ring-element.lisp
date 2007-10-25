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

(defclass ring-element ()
  (base-ring)
  (:documentation "Base class for ring elements."))

(defgeneric ring-copy (element)
  (:documentation "Returns a deep copy of an element"))

(defgeneric ring-zero-p (element)
  (:documentation "Returns t if element is zero, nil otherwise"))

(defgeneric ring-equal-p (e1 e2)
  (:documentation "Returns t if e1 equals e2, nil otherwise"))

(defgeneric ring-identity-p (element)
  (:documentation "Returns t if element is the multiplicative
  identity, nil otherwise"))

(defgeneric ring+ (element &rest more-elements))
(defgeneric ring- (element &rest more-elements))
(defgeneric ring* (element &rest more-elements))
(defgeneric ring/ (element &rest more-elements))
(defgeneric ring-mod (element &rest more-elements))

(defgeneric add (e1 e2)
  (:documentation "Adds ring elements"))

(defgeneric sub (e1 e2)
  (:documentation "Subtracts ring elements"))

(defgeneric mul (e1 e2)
  (:documentation "Multiplies ring elements"))

(defgeneric dividesp (e1 e2)
  (:documentation "Returns t if e1 divides e2 in the base ring"))

(defgeneric div (e1 e2)
  (:documentation "Divides ring elements"))

(defgeneric divmod (element divisors)
  (:documentation "Returns quotient(s) and remainder if we are working
  in an Euclidean ring."))

(defgeneric element->string (element &key &allow-other-keys)
  (:documentation "Returns a human-readable string representation of
  an element"))

(defgeneric ring-lcm (e1 e2)
  (:documentation "Returns the LCM of e1 and e2"))
