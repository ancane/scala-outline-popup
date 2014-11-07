;;; scala-outline-popup.el --- scala file summary popup

;; Copyright (C) 2014 <igor.shimko@gmail.com>

;; Author: Igor Shymko <igor.shimko@gmail.com>
;; Version: 0.1
;; Package-Requires: ((popup "0.5.0") (scala-mode2 "0.22"))
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

(require 'popup)
(require 'scala-mode2)

(defun scala-outline-tags ()
  (let ((tags-list nil)
        (tag-re "\\b\\(class\\|trait\\|object\\|type\\|def\\|implicit[ \t]+val\\)\\b"))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward (concat "^[^\n\\/*{]*" tag-re "[ \t]+\\([^\n]+\\)$") nil t)
        (setq tags-list
              (cons
               (list
                (buffer-substring-no-properties (point) (scala-outline-tag-end tag-re))
                (line-number-at-pos))  tags-list))))
    tags-list))

(defun scala-outline-tag-end (tag-re)
  (save-excursion
    (beginning-of-line)
    (scala-syntax:forward-modifiers)
    (re-search-forward tag-re nil t)
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
             (selected (popup-menu*
                        popup-items
                        :point (point)
                        :height menu-height
                        :isearch t
                        :scroll-bar t
                        :margin-left 1
                        :margin-right 1
                        :around nil
                        )))
        (goto-line (car (cdr selected)))
        (search-forward (car selected))
        (re-search-backward "[ \t]")
        (forward-char))
    (message "Not in scala mode")))

(provide 'scala-outline-popup)

;;; scala-outline-popup.el ends here
