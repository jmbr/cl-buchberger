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

(defun degree (m)
  "Returns the total degree of a monomial"
  (reduce #'+ m))

(defun lex> (m1 m2)
  "Lexicographic Order"
  (let ((v (vector- m1 m2)))
    (when (not (vector-zero-p v))
      (>= (find-if-not #'zerop v) 0))))

(defun grlex> (m1 m2)
  "Graded Lex Order"
  (let ((d1 (degree m1))
	(d2 (degree m2)))
    (or (> d1 d2)
	(and (= d1 d2)
	     (lex> m1 m2)))))

(defun grevlex> (m1 m2)
  "Graded Reverse Lex Order"
  (let ((d1 (degree m1))
	(d2 (degree m2))
	(v (vector- m1 m2)))
    (or (> d1 d2)
	(and (= d1 d2)
	     (when (not (vector-zero-p v))
	       (minusp (find-if-not #'zerop v :from-end t)))))))

(defvar *monomial-ordering* #'lex>
  "Specifies the ordering of monomials in a polynomial")

(defmacro with-monomial-ordering (ordering &body body)
  `(let ((*monomial-ordering* ,ordering))
     ,@body))
