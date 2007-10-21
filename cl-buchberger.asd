
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
  :description "CL-Buchberger: A Common Lisp implementation of Buchberger's algorithm."
  :version "0.0.1"
  :author "Juan M. Bello Rivas <jmbr@superadditive.com>"
  :license "GNU GPLv2"
  :components ((:file "package")
               (:file "vector" :depends-on ("package"))
               (:file "ring" :depends-on ("package"))
	       (:file "ring-element" :depends-on ("package" "ring"))
	       (:file "term" :depends-on ("package" "ring-element"))
               (:file "monomial-orderings" :depends-on ("package" "vector"))
	       (:file "polynomial-ring"
                      :depends-on ("package" "ring" "ring-element"))
	       (:file "polynomial"
                      :depends-on ("package" "term" "monomial-orderings" "polynomial-ring"))
	       (:file "arithmetic" :depends-on ("package" "polynomial"))
	       (:file "groebner" :depends-on ("package" "monomial-orderings" "arithmetic"))))
