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
  "Returns the sum of two vectors."
  (map 'vector #'+ v1 v2))

(defun vector- (v1 v2)
  "Returns the difference of two vectors."
  (map 'vector #'- v1 v2))

(defun vector-zero-p (v)
  "Returns t if every compoment in v is zero."
  (every #'zerop v))

(defun vector= (v1 v2)
  "Returns t if both vectors are equal, nil otherwise."
  (equalp v1 v2))

(defun vector> (v1 v2)
  "Returns t if every component in v1 is greater than the
corresponding component in v2, nil otherwise."
  (notany #'zerop (vector- v1 v2)))
