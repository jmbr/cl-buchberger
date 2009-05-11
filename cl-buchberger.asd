
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

(defpackage :com.superadditive.cl-buchberger-system
  (:use :common-lisp :asdf))

(in-package :com.superadditive.cl-buchberger-system)

(defsystem "cl-buchberger"
  :description "cl-buchberger: A Common Lisp implementation of Buchberger's algorithm."
  :version "0.0.3"
  :author "Juan M. Bello Rivas <jmbr@superadditive.com>"
  :license "GNU GPLv2"
  :serial t
  :components ((:file "package")
               (:file "vector")
               (:file "ring")
	       (:file "ring-element")
	       (:file "term")
               (:file "monomial-orderings")
	       (:file "polynomial-ring")
	       (:file "polynomial")
	       (:file "arithmetic")
	       (:file "groebner")
               (:file "ideal")))
