;;; scala-outline-popup.el --- scala file summary popup

;; Copyright (C) 2014 <igor.shimko@gmail.com>

;; Author: Igor Shymko <igor.shimko@gmail.com>
;; Version: 0.1
;; Package-Requires: ((dash "2.9.0") (popup "20141106.455") (scala-mode2 "0.22"))
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

(eval-when-compile (require 'cl))

(require 'dash)
(require 'popup)
(require 'scala-mode2)

(defvar scala-outline-popup-selection nil
  "Makes popup pre-select an item.
   First item is selected by default.
   Possible values are one of:
   'closest - an item, closest to the point
   'next    - successor item starting at point
   'prev    - predecessor starting at point
")

(setq scala-outline-popup-selection 'prev)

(defconst scala-outline-popup-tag-re "\\b\\(class\\|trait\\|object\\|type\\|def\\|implicit[ \t]+\\(lazy[ \t]+\\)?val\\)\\b")

(defconst scala-outline-popup-item-re (concat "^[^\n\\/*{]*" scala-outline-popup-tag-re "[ \t]+\\([^\n]+\\)$"))

(defun scala-outline-popup-index (items selection)
  (cond
   ((eq selection 'closest)
    (scala-popup-outline-item-index-by-line
     items
     (if
         (save-excursion
           (beginning-of-line)
           (looking-at scala-outline-popup-item-re))
         (line-number-at-pos)
       (-min-by
        (lambda (it other)
          (>= (abs (- it (line-number-at-pos))) (abs (- other (line-number-at-pos))) ))
        (list (scala-outline-popup-next-tag-line) (scala-outline-popup-prev-tag-line))))))
   ((eq selection 'next)
    (scala-popup-outline-item-index-by-line
     items
     (scala-outline-popup-next-tag-line)))
   ((eq selection 'prev)
    (scala-popup-outline-item-index-by-line
     items
     (scala-outline-popup-prev-tag-line)))
   (t 0)))

(defun scala-popup-outline-item-index-by-line (items line)
  (-find-index (lambda (item) (eq line (car (cdr item)))) items))

(defun scala-outline-popup-next-tag-line ()
  (save-excursion
    (beginning-of-line)
    (re-search-forward scala-outline-popup-item-re nil t)
    (line-number-at-pos)))

(defun scala-outline-popup-prev-tag-line ()
  (save-excursion
    (beginning-of-line)
    (re-search-backward scala-outline-popup-item-re nil t)
    (line-number-at-pos)))

(defun scala-outline-tags ()
  (let ((tags-list nil))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward scala-outline-popup-item-re nil t)
        (setq tags-list
              (cons
               (list
                (buffer-substring-no-properties (point) (scala-outline-tag-end))
                (line-number-at-pos))  tags-list))))
    tags-list))

(defun scala-outline-tag-end ()
  (save-excursion
    (beginning-of-line)
    (scala-syntax:forward-modifiers)
    (re-search-forward scala-outline-popup-tag-re nil t)
    (scala-syntax:forward-sexp)
    (point)))

;;;###autoload
(defun scala-outline-popup ()
  (interactive)
  (if (equal major-mode 'scala-mode)
      (let* (
             (popup-list (scala-outline-tags))
             (menu-height (min 15 (length popup-list) (- (window-height) 4)))
             (popup-items (mapcar (lambda (x)
                                    (popup-make-item
                                     (car x)
                                     :value x))
                                  popup-list))
             (selection-index
              (scala-outline-popup-index
               popup-list
               scala-outline-popup-selection))

             (selected (popup-menu*
                        popup-items
                        :point (point)
                        :height menu-height
                        :isearch t
                        :scroll-bar t
                        :margin-left 1
                        :margin-right 1
                        :initial-index selection-index
                        :around nil
                        )))
        (goto-line (car (cdr selected)))
        (search-forward (car selected))
        (re-search-backward "[ \t]")
        (forward-char))
    (message "Not in scala mode")))

(provide 'scala-outline-popup)

;;; scala-outline-popup.el ends here
