;;; describe-syntax-ppss.el --- Visualize syntax ppss at point -*- lexical-binding: t -*-

;; Copyright © 2020-2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/describe-syntax-ppss
;; Keywords: lisp
;; Version: 0.1.1
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Show annotated result of calling syntax-ppss at point in minibuffer.

;; Usage:

;; M-x `describe-syntax-ppss-at-point'

;;; Code:

(defvar-local describe-syntax-ppss-info nil)
(defvar-local describe-syntax-ppss-position nil)

(defvar describe-syntax-ppss-actions
  `(("depth" . identity)
    ("start of nested list" . describe-syntax-ppss-jump-and-mark)
    ("start of last complete sexp" . describe-syntax-ppss-jump-and-mark)
    (,(propertize "inside string" 'face 'font-lock-string-face)
     . describe-syntax-ppss-print-char)
    ("inside comment" . identity)
    ("following pos quote" . describe-syntax-ppss-jump-and-mark)
    ("paren-depth" . identity)
    ("style of comment" . describe-syntax-ppss-print-char)
    ("start of comment/string" .
     describe-syntax-ppss-highlight-comment-or-string)
    ("positions of currently open parens" .
     describe-syntax-ppss-visit-open-parens)
    ("two character comment delimiter" .
     describe-syntax-ppss-jump-and-mark))
  "Alist of syntax pps descriptions and actions.")

(defun describe-syntax-ppss-print-char (char)
  "Convert arg CHAR to a string containing that character and show it."
  (message (char-to-string char)))

(defun describe-syntax-ppss-highlight-comment-or-string (position)
  "Highlight comment or string starting at POSITION.
POSITION should be start of comment of string."
  (let ((ppss (syntax-ppss position)))
    (save-excursion
      (cond ((nth 4 ppss)
             (progn (goto-char position)
                    (forward-comment 1)
                    (pulse-momentary-highlight-region position (point))))
            ((nth 3 ppss)
             (goto-char position)
             (forward-sexp 1)
             (pulse-momentary-highlight-region position (point)))))))

(defun describe-syntax-ppss-visit-open-parens (positions)
  "Incrementally highlight lists at POSITIONS."
  (let (beg (next t))
    (while (and (setq beg (pop positions))
                next)
      (goto-char beg)
      (save-excursion
        (forward-list 1)
        (pulse-momentary-highlight-region beg (point)))
      (setq next (when positions
                   (yes-or-no-p "Next?"))))))

(defun describe-syntax-ppss-stringify (x)
  "Convert X to string effeciently.
X can be any object."
  (cond
   ((stringp x)
    x)
   ((symbolp x)
    (symbol-name x))
   ((integerp x)
    (number-to-string x))
   ((floatp x)
    (number-to-string x))
   (t (format "%s" x))))

(defun describe-syntax-ppss-get-prop (item property)
  "Get PROPERTY from ITEM.
ITEM can be propertized string or plist."
  (if (stringp item)
      (get-text-property 0 property item)
    (when (listp item)
      (plist-get item property))))

(defun describe-syntax-ppss-add-props (string &rest properties)
  "Propertize STRING with PROPERTIES."
  (setq string (describe-syntax-ppss-stringify string))
  (let* ((result (list 'head))
         (last result))
    (while properties
      (let* ((key (pop properties))
             (val (pop properties))
             (new (and val (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (apply #'propertize string (cdr result))))

(defun describe-syntax-ppss-jump-and-mark (position)
  "Jump to POSITION and mark sexp."
  (goto-char position)
  (mark-sexp))

;;;###autoload
(defun describe-syntax-ppss-kill-cursor-position (&optional detail)
  "Same as `what-cursor-position' but also copies position.
In addition, with prefix argument DETAIL, show details about that character
in *Help* buffer."
  (interactive "P")
  (let* ((str (funcall-interactively #'what-cursor-position detail))
         (pos (with-temp-buffer
                (insert str)
                (re-search-backward "point=\\([0-9]+\\)" nil t 1)
                (match-string-no-properties 1))))
    (kill-new pos)
    (string-to-number pos)))

;;;###autoload
(defun describe-syntax-ppss-at-point ()
  "Show annotated result of `syntax-ppss' at point in minibuffer.
Perfom action for selected choice defined in `describe-syntax-ppss-actions'."
  (interactive)
  (let ((buffer (current-buffer))
        (prompt)
        (choices (mapcar #'car describe-syntax-ppss-actions))
        (result))
    (with-current-buffer buffer
      (setq describe-syntax-ppss-position (point))
      (setq describe-syntax-ppss-info
            (syntax-ppss describe-syntax-ppss-position))
      (setq prompt (format "Syntax pps at %s\s"
                           describe-syntax-ppss-position))
      (setq choices
            (seq-map-indexed
             (lambda (str idx)
               (let*
                   ((value (nth idx describe-syntax-ppss-info))
                    (face (if (null value)
                              'font-lock-comment-face
                            (or (describe-syntax-ppss-get-prop str 'face)
                                'font-lock-preprocessor-face)))
                    (text (concat (format "%s %s:\s" idx
                                          (propertize
                                           str
                                           'face
                                           face))
                                  (format "%s" (or value "")))))
                 (describe-syntax-ppss-add-props text :value value
                                                 :idx idx)))
             choices)))
    (setq result (completing-read prompt choices))
    (when-let ((pos (describe-syntax-ppss-get-prop result :value))
               (action (cdr (nth
                             (describe-syntax-ppss-get-prop result :idx)
                             describe-syntax-ppss-actions))))
      (funcall action pos))))

(defvar-local describe-syntax-ppss-text-props-last-pos nil)

(defun describe-syntax-ppss-text-props-at-point ()
  "Display text properties and syntax state at point."
  (let ((pos (point)))
    (unless
        (and describe-syntax-ppss-text-props-last-pos
             (= pos
                describe-syntax-ppss-text-props-last-pos))
      (when-let ((wnd (get-buffer-window (current-buffer))))
        (when (eq wnd (selected-window))
          (setq describe-syntax-ppss-text-props-last-pos pos)
          (unless (>= pos (point-max))
            (with-selected-window wnd
              (describe-text-properties pos))))))))

;;;###autoload
(define-minor-mode describe-syntax-ppss-text-props-mode
  "Toggle displaying text properties and syntax state at point after commands."
  :lighter " descr-props"
  :global nil
  (if describe-syntax-ppss-text-props-mode
      (add-hook 'post-command-hook #'describe-syntax-ppss-text-props-at-point
                nil 'local)
    (remove-hook 'post-command-hook #'describe-syntax-ppss-text-props-at-point
                 'local)))

(provide 'describe-syntax-ppss)
;;; describe-syntax-ppss.el ends here
