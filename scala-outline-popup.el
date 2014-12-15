;;; scala-outline-popup.el --- scala file summary popup

;; Copyright (C) 2014 <igor.shimko@gmail.com>

;; Author: Igor Shymko <igor.shimko@gmail.com>
;; Version: 0.3.4
;; Package-Requires: ((dash "2.9.0") (popup "0.5.2") (scala-mode2 "0.22"))
;; Keywords: scala, structure, summary
;; URL: https://github.com/ancane/scala-outline-popup.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Opens a popup window containing classes, objects, types,
;; defs and implicit vals from active scala file.
;; Keeps indentation of all the items. List is filterable.
;; `Enter` on an item jumps to it's position in file.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))

(require 'dash)
(require 'popup)
(require 'scala-mode2)

(defvar scala-outline-popup-select nil
  "Makes popup pre-select an item.
   First item is selected by default.
   Possible values are one of:
   'closest - an item, closest to the point
   'next    - successor item starting at point
   'prev    - predecessor starting at point")

(defconst scalop--def-re "\\b\\(class\\|trait\\|object\\|type\\|def\\|implicit[ \t]+\\(lazy[ \t]+\\)?val\\)\\b")

(defconst scalop--line-def-re (concat "^[^\n\\/*{|\"]*" scalop--def-re "[ \t]+\\([^\n]+\\)$"))

(defun scalop--looking-at-def ()
  (save-excursion
    (beginning-of-line)
    (looking-at scalop--line-def-re)))

(defun scalop--def-index (items search)
  (let ((linum (line-number-at-pos)))
    (cond
     ((eq search 'closest)
      (scalop--def-index-by-line
       items
       (if (scalop--looking-at-def)
           linum
         (let ((candidates (-non-nil (list
                                      (scalop--next-def-line)
                                      (scalop--prev-def-line)
                                      ))))
           (if candidates
               (-min-by
                (lambda (it other)
                  (>=
                   (abs (- it    linum))
                   (abs (- other linum))))
                candidates)
             linum)))))
     ((eq search 'next)
      (scalop--def-index-by-line
       items
       (car (-non-nil
             (list
              (scalop--next-def-line)
              linum)))))
     ((eq search 'prev)
      (scalop--def-index-by-line
       items
       (car (-non-nil
             (list
              (scalop--prev-def-line)
              linum)))))
     (t 0))))

(defun scalop--def-index-by-line (items line)
  (-find-index
   (lambda (item) (eq line (car (cdr item))))
   items))

(defun scalop--next-def-line ()
  (save-excursion
    (end-of-line)
    (when (re-search-forward scalop--line-def-re nil t)
      (line-number-at-pos))))

(defun scalop--prev-def-line ()
  (save-excursion
    (beginning-of-line)
    (when (re-search-backward scalop--line-def-re nil t)
      (line-number-at-pos))))

(defun scalop--defs-list ()
  (let ((defs-list nil))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward scalop--line-def-re nil t)
        (setq defs-list
              (cons
               (list
                (buffer-substring-no-properties (point) (scalop--def-end-pos))
                (line-number-at-pos))
               defs-list))))
    defs-list))

(defun scalop--def-end-pos ()
  (save-excursion
    (beginning-of-line)
    (scala-syntax:forward-modifiers)
    (re-search-forward scalop--def-re nil t)
    (scala-syntax:forward-sexp)
    (point)))

;;;###autoload
(defun scala-outline-popup ()
  (interactive)
  (if (equal major-mode 'scala-mode)
      (let* (
             (popup-list (scalop--defs-list))
             (menu-height (min 15 (length popup-list) (- (window-height) 4)))
             (popup-items (mapcar (lambda (x) (popup-make-item
                                               (car x)
                                               :value x))
                                  popup-list))
             (def-index
              (scalop--def-index popup-list scala-outline-popup-select))

             (selected (popup-menu*
                        popup-items
                        :point (point)
                        :height menu-height
                        :isearch t
                        :scroll-bar t
                        :margin-left 1
                        :margin-right 1
                        :initial-index def-index
                        :around nil
                        )))
        (goto-line (car (cdr selected)))
        (search-forward (car selected))
        (re-search-backward "[ \t]")
        (forward-char))
    (message "Not in scala mode")))

(provide 'scala-outline-popup)

;;; scala-outline-popup.el ends here
