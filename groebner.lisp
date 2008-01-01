(in-package :com.superadditive.cl-buchberger)

;; Copyright (C) 2007  Juan M. Bello Rivas <jmbr@superadditive.com>
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

(defun s-poly (f g)
   "Returns the S-polynomial of f and g"
   ;; S(f, g) = x^gamma/LT(f) * f - x^gamma/LT(g) * g where x^gamma is
   ;; LCM(LM(f), LM(g))
   (flet ((make-x-gamma (f g)
            (make-instance 'term
                           :coefficient 1
                           :monomial (map 'vector #'max
                                          (monomial (lt f)) (monomial (lt g))))))
     (let ((x-gamma (make-x-gamma f g)))
       (sub (mul f (div x-gamma (lt f)))
            (mul g (div x-gamma (lt g)))))))

(defun make-index-set (s)
  (loop for i below s nconc
       (loop for j below s when (< i j) collect (cons i j))))

(defun pair-member (l m B)
  (or (member (cons l m) B :test #'equal)
      (member (cons m l) B :test #'equal)))

(defun criterion (G B i j fi fj)
  "Returns t if S_{ij} has to be considered"
  (dotimes (k (length G))
    (let ((fk (aref G k)))
      (if (and (/= k i)
               (/= k j)
               (dividesp (lt fk)
                         (ring-lcm (lt fi) (lt fj))))
          (if (and (not (pair-member i k B))
                   (not (pair-member j k B)))
              (return t))))))

(defun normal-form (f G)
  (nth-value 1 (divmod f (remove-if #'ring-zero-p G))))

(defun groebner (polynomials)
  "Returns a Groebner basis for the ideal generated by the specified
  polynomials"
  (declare (optimize (safety 3)))
  (let* ((s (length polynomials))
         (r s)
         (G (make-array s :initial-contents polynomials
                        :adjustable t :fill-pointer s)))
    (do ((B (make-index-set s) (rest B)))
        ((null B) G)
      (destructuring-bind (i . j) (first B)
        (let ((fi (elt G i))
              (fj (elt G j)))
           (if (and (not (ring-equal-p (ring-lcm (lt fi) (lt fj))
                                       (mul (lt fi) (lt fj))))
                    (not (criterion G B i j fi fj)))
               (let ((remainder (normal-form (s-poly fi fj) G)))
                 (unless (ring-zero-p remainder)
                   (vector-push-extend remainder G)
                   (setf B (append B (loop for i below r collect (cons i r))))
                   (incf r)))))))))

(defun reduce-gb (G)
  "Returns a reduced Groebner basis."
  (flet ((filter (f G)
           (remove-if #'(lambda (x) (or (ring-zero-p x) (equal f x))) G)))
    (loop
       with F = (map 'vector #'(lambda (x) (mul x (1/ (lc x)))) G)
       for i below (length F) for fi across F do
         (setf (elt F i)
               (if (some #'(lambda (x) (dividesp (lt x) (lt fi)))
                         (filter fi F))
                   (make-instance 'polynomial :ring (base-ring fi)) ; zero polynomial
                   (normal-form fi (remove fi F))))
       finally (return (remove-if #'ring-zero-p F)))))

;;; TODO: Gröbner basis reductions would be faster if they were
;;; intertwined with the basis computation.
(defun reduced-groebner (F)
  "Computes and reduces a Groebner basis of the ideal generated by F."
  (reduce-gb (groebner F)))
