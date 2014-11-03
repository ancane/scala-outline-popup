;;; scala-outline-popup.el --- scala file structure outline popup.

;; Copyright (C) 2014 <igor.shimko@gmail.com>

;; Author: Igor Shymko <igor.shimko@gmail.com>
;; URL: https://github.com/ancane/markdown-preview-mode
;; Keywords: scala, outline, popup

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

(require 's)
(require 'dash)
(require 'popup)


(defun scala-outline-popup (&optional file)
  (interactive)
  (let* (
         (scala-re "/^[^\n\\/(*]*\\(class\\|trait\\|object\\|type\\|def\\)[ \t]+\\([^\n]+\\)/\1/")
         (shell-string
          (shell-command-to-string
           (format "etags -f - --regex=\"%s\" %s" scala-re (if file file (buffer-file-name)))))
         (tags-list
          (cdr (-remove (lambda (x) (string-equal "" x))
                         (split-string shell-string "\n"))))
         (file-name-str (car tags-list))
         (popup-list
          (-map (lambda (x) (list (scala-outline-trim-popup-item (car x)) (car (cdr x))))
                (-remove (lambda (x) (not x))
                         (-map (lambda (x)
                                 (scala-outline-popup-parse-etag x)) (cdr tags-list)))))
         (menu-height (min 15 (length popup-list) (- (window-height) 4)))
         (menu-x (/ (- fill-column
                       (if popup-list
                           (apply 'max (mapcar (lambda (x) (length (car x))) popup-list))
                         0)
                       ) 2))
         (menu-y (+ (- (line-number-at-pos (window-start)) 2) (/ (- (window-height) menu-height) 2)))
         (popup-items
          (-map (lambda (x)
                  (popup-make-item
                   (car x)
                   :value x)) popup-list))
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
    (forward-char)
    )
  )

(defun scala-outline-popup-parse-etag (x)
  (when (string-match "^\\(.*?\\)\^?\^A\^A\\([0-9]+\\),\\([0-9]+\\)$" x)
    (list
     (match-string 1 x)
     (string-to-number (match-string 2 x)))))

(defun scala-outline-trim-popup-item (x)
  (let ((trimmed (car (s-slice-at "\\([ \t]*\\(extends\\|[[]\\|\(\\|{\\)\\)" x))))
    (if (string-match "^\\(.*def[ \t]+.+\\)\\(:\\|[ ]+=\\).+$" trimmed)
        (match-string 1 trimmed)
      trimmed
      )
    )
  )

(provide 'scala-outline-popup)

;;; scala-outline-popup.el ends here
