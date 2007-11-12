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

(defun vector+ (v1 v2)
  "Returns the sum of the two vectors V1 and V2."
  (map 'vector #'+ v1 v2))

(defun vector- (v1 v2)
  "Returns the difference of the two vectors V1 and V2."
  (map 'vector #'- v1 v2))

(defun vector-zero-p (v)
  "Returns T if every compoment in V is zero."
  (every #'zerop v))

(defun vector= (v1 v2)
  "Returns T if both vectors V1 and V2 have the same components, NIL
otherwise."
  (equalp v1 v2))

(defun vector> (v1 v2)
  "Returns T if every component in V1 is greater than the
corresponding component in V2, NIL otherwise."
  (notany #'zerop (vector- v1 v2)))
