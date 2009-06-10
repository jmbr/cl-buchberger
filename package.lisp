
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

(cl:in-package :cl-user)

(cl:defpackage :com.superadditive.cl-buchberger
  (:nicknames :cl-buchberger)
  (:use :common-lisp :asdf)
  (:export #:polynomial-ring
           #:polynomial #:*ring* #:with-polynomial-ring
           #:ring+ #:ring- #:ring* #:ring/
           #:ring-zero-p #:ring-equal-p #:ring-identity-p #:ring-lcm
           #:degree #:lex> #:grlex> #:grevlex> #:*monomial-ordering* #:with-monomial-ordering
           #:lt #:lm #:lc #:multideg #:make-polynomial #:doterms #:mapterm
           #:s-poly #:groebner #:reduce-gb #:reduced-groebner
           #:make-ideal #:basis #:member-p))
