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

(defmethod ring+ ((poly polynomial) &rest more-polynomials)
  (reduce #'add more-polynomials :initial-value poly))

(defmethod ring- ((poly polynomial) &rest more-polynomials)
  (reduce #'sub more-polynomials :initial-value poly))

(defmethod ring* ((poly polynomial) &rest more-polynomials)
  (reduce #'mul more-polynomials :initial-value poly))

(defmethod ring/ ((poly polynomial) &rest more-polynomials)
  (divmod poly
          (make-array (length more-polynomials)
                      :initial-contents more-polynomials)))

(defmethod add ((poly polynomial) (tm term))
  "Returns the polynomial with the added term"
  (let ((new-poly (ring-copy poly)))
    (with-slots (terms) new-poly
      (with-slots (coefficient monomial) tm
        (let* ((old-value (gethash monomial terms 0))
               (new-value (+ old-value coefficient)))
          (if (zerop new-value)
              (remhash monomial terms)
              (setf (gethash monomial terms) new-value)))))
    new-poly))

(defmethod add ((p1 polynomial) (p2 polynomial))
  "Returns the sum of two polynomials"
  (let ((new-poly (ring-copy p1)))
    (doterms (tm p2 new-poly)
      (setf new-poly (add new-poly tm)))))

(defmethod sub ((poly polynomial) (tm term))
  "Returns the result of subtracting the term from the polynomial"
  (with-slots (coefficient monomial) tm
    (add poly (make-instance 'term
                             :coefficient (* -1 coefficient)
                             :monomial monomial))))

(defmethod sub ((p1 polynomial) (p2 polynomial))
  "Returns the result of substracting two polynomials."
  (add p1
       (mul p2
            (make-instance 'term
                           :ring (base-ring p2)
                           :coefficient -1))))

(defmethod mul ((poly polynomial) (num number))
  (let ((new-poly (make-instance 'polynomial :ring (base-ring poly))))
    (doterms (tt poly new-poly)
      (setf new-poly (add new-poly (mul tt num))))))

(defmethod mul ((t1 term) (num number))
  (make-instance 'term
                 :coefficient (* num (coefficient t1))
                 :monomial (monomial t1)))

(defmethod mul ((t1 term) (t2 term))
  "Multiplies two terms storing the result in the first term."
  (with-slots ((c1 coefficient) (m1 monomial)) t1
    (with-slots ((c2 coefficient) (m2 monomial)) t2
      (make-instance 'term
                     :coefficient (* c1 c2)
                     :monomial (map 'vector #'+ m1 m2)))))

(defmethod mul ((poly polynomial) (tm term))
  "Returns the product of a polynomial by a term"
  (let ((new-poly (make-instance 'polynomial :ring (base-ring poly))))
    (doterms (tt poly new-poly)
      (setf new-poly (add new-poly (mul tt tm))))))

(defmethod mul ((p1 polynomial) (p2 polynomial))
  "Returns the product of two polynomials"
  (assert (and p1 p2))
  (let ((new-poly (make-instance 'polynomial :ring (base-ring p1))))
    (doterms (tm p2 new-poly)
      (setf new-poly (add new-poly (mul p1 tm))))))

(defmethod dividesp ((t1 term) (t2 term))
  (assert (and t1 t2))
  (with-slots ((c1 coefficient) (m1 monomial)) t1
    (with-slots ((c2 coefficient) (m2 monomial)) t2
      ;; We don't have to check (dividesp c1 c2) because we're working
      ;; with polynomials over a *field*
      (and (not (zerop c1))
           (every #'>= m2 m1)))))

(defmethod dividesp ((t1 term) (p polynomial))
  (assert (and t1 p))
  (every #'identity
         (mapterm #'(lambda (x) (dividesp t1 x)) p)))

(defmethod div ((t1 term) (t2 term))
  "Returns the quotient of two terms"
  (assert (dividesp t2 t1))
  (with-slots ((c1 coefficient) (m1 monomial)) t1
    (with-slots ((c2 coefficient) (m2 monomial)) t2
      (make-instance 'term
                     :coefficient (/ c1 c2)
                     :monomial (vector- m1 m2)))))

(defmethod divmod ((f polynomial) fs)
  "Divides f by the polynomials in the sequence fs and returns the
quotients (as an array) and a remainder."
  (flet ((init-vector (poly n)
           (let ((vs (make-array n :fill-pointer 0)))
             (dotimes (i n vs)
               (vector-push (make-instance 'polynomial
                                           :ring (base-ring poly)) vs)))))
    (loop
       with p = (ring-copy f)
       with len-fs = (length fs)
       with as = (init-vector f len-fs)
       with r = (make-instance 'polynomial :ring (base-ring f))
       while (not (ring-zero-p p)) do
         (let ((division-occurred-p nil))
           (loop
              with i = 0
              while (and (< i len-fs)
                         (not division-occurred-p)) do
              (let ((fi (elt fs i)))
                (if (dividesp (lt fi) (lt p))
                    (progn
                      (setf (elt as i)
                            (add (elt as i) (div (lt p) (lt fi))))
                      (setf p (sub p (mul fi (div (lt p) (lt fi)))))
                      (setf division-occurred-p t))
                    (incf i))))
           (if (not division-occurred-p)
               (progn
                 (setf r (add r (lt p)))
                 (setf p (sub p (lt p))))))
       finally (return (values as r)))))

(defmethod ring-mod (f &rest fs)
  (nth-value 1 (apply #'divmod f fs)))

(defmethod ring-lcm ((t1 term) (t2 term))
  "Returns LCM(t1, t2)"
  (with-slots ((c1 coefficient) (m1 monomial)) t1
    (with-slots ((c2 coefficient) (m2 monomial)) t2
      (make-instance 'term
                     :coefficient (ring-lcm c1 c2)
                     :monomial (map 'vector #'max m1 m2)))))

(defmethod ring-lcm ((r1 rational) (r2 rational))
  "LCM over the rationals."
  ;; Translated from SAGE.
  (let ((d (* (denominator r1) (denominator r2)))
        (r1_d (* (numerator r1) (denominator r2)))
        (r2_d (* (numerator r2) (denominator r1))))
    (/ (lcm r1_d r2_d) d)))

(defun 1/ (number)
  "Return the inverse of number"
  (/ 1 number))
