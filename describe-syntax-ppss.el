;;; describe-syntax-ppss.el --- Visualize syntax ppss at point -*- lexical-binding: t -*-

;; Copyright © 2020-2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/describe-syntax-ppss
;; Keywords: lisp
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

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

(declare-function color-saturate-name "color")
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

(defun describe-syntax-ppss--minibuffer-get-metadata ()
  "Return current minibuffer completion metadata."
  (completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end)
         (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun describe-syntax-ppss--minibuffer-ivy-selected-cand ()
  "Return the currently selected item in Ivy."
  (when (and (memq 'ivy--queue-exhibit post-command-hook)
             (boundp 'ivy-text)
             (boundp 'ivy--length)
             (boundp 'ivy-last)
             (fboundp 'ivy--expand-file-name)
             (fboundp 'ivy-state-current))
    (cons
     (completion-metadata-get (ignore-errors (describe-syntax-ppss--minibuffer-get-metadata))
                              'category)
     (ivy--expand-file-name
      (if (and (> ivy--length 0)
               (stringp (ivy-state-current ivy-last)))
          (ivy-state-current ivy-last)
        ivy-text)))))

(defun describe-syntax-ppss--minibuffer-get-default-candidates ()
  "Return all current completion candidates from the minibuffer."
  (when (minibufferp)
    (let* ((all (completion-all-completions
                 (minibuffer-contents)
                 minibuffer-completion-table
                 minibuffer-completion-predicate
                 (max 0 (- (point)
                           (minibuffer-prompt-end)))))
           (last (last all)))
      (when last (setcdr last nil))
      (cons
       (completion-metadata-get (describe-syntax-ppss--minibuffer-get-metadata) 'category)
       all))))

(defun describe-syntax-ppss--get-minibuffer-get-default-completion ()
  "Target the top completion candidate in the minibuffer.
Return the category metadatum as the type of the target."
  (when (and (minibufferp) minibuffer-completion-table)
    (pcase-let* ((`(,category . ,candidates)
                  (describe-syntax-ppss--minibuffer-get-default-candidates))
                 (contents (minibuffer-contents))
                 (top (if (test-completion contents
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate)
                          contents
                        (let ((completions (completion-all-sorted-completions)))
                          (if (null completions)
                              contents
                            (concat
                             (substring contents
                                        0 (or (cdr (last completions)) 0))
                             (car completions)))))))
      (cons category (or (car (member top candidates)) top)))))

(defvar describe-syntax-ppss--minibuffer-targets-finders
  '(describe-syntax-ppss--minibuffer-ivy-selected-cand
    describe-syntax-ppss--get-minibuffer-get-default-completion))

(defun describe-syntax-ppss--minibuffer-get-current-candidate ()
  "Return cons filename for current completion candidate."
  (let (target)
    (run-hook-wrapped
     'describe-syntax-ppss--minibuffer-targets-finders
     (lambda (fun)
       (when-let ((result (funcall fun)))
         (when (and (cdr-safe result)
                    (stringp (cdr-safe result))
                    (not (string-empty-p (cdr-safe result))))
           (setq target result)))
       (and target (minibufferp))))
    target))

(defun describe-syntax-ppss--minibuffer-exit-with-action (action)
  "Call ACTION with current candidate and exit minibuffer."
  (pcase-let ((`(,_category . ,current)
               (describe-syntax-ppss--minibuffer-get-current-candidate)))
    (progn (run-with-timer 0.1 nil action current)
           (abort-minibuffers))))


(defun describe-syntax-ppss--minibuffer-action-no-exit (action)
  "Call ACTION with minibuffer candidate in its original window."
  (pcase-let ((`(,_category . ,current)
               (describe-syntax-ppss--minibuffer-get-current-candidate)))
    (with-minibuffer-selected-window
      (funcall action current))))


(defun describe-syntax-ppss--completing-read-with-preview (prompt collection
                                                                  &optional preview-action keymap predicate require-match
                                                                  initial-input hist def inherit-input-method)
  "Read COLLECTION in minibuffer with PROMPT and KEYMAP.
See `completing-read' for PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
INHERIT-INPUT-METHOD."
  (let ((collection (if (stringp (car-safe collection))
                        (copy-tree collection)
                      collection)))
    (minibuffer-with-setup-hook
        (lambda ()
          (when (minibufferp)
            (when keymap
              (let ((map (make-composed-keymap keymap
                                               (current-local-map))))
                (use-local-map map)))
            (when preview-action
              (add-hook 'after-change-functions (lambda (&rest _)
                                                  (interactive)
                                                  (describe-syntax-ppss--minibuffer-action-no-exit
                                                   preview-action))
                        nil t))))
      (completing-read prompt
                       collection
                       predicate
                       require-match initial-input hist
                       def inherit-input-method))))
(defvar-local describe-syntax-ppss-overlay nil)
(defun describe-syntax-ppss--overlay-make (start end &optional buffer
                                                 front-advance rear-advance
                                                 &rest props)
  "Create a new overlay with range BEG to END in BUFFER and return it.
If omitted, BUFFER defaults to the current buffer.
START and END may be integers or markers.
The fourth arg FRONT-ADVANCE, if non-nil, makes the marker
for the front of the overlay advance when text is inserted there
\(which means the text *is not* included in the overlay).
The fifth arg REAR-ADVANCE, if non-nil, makes the marker
for the rear of the overlay advance when text is inserted there
\(which means the text *is* included in the overlay).
PROPS is a plist to put on overlay."
  (let ((overlay (make-overlay start end buffer front-advance
                               rear-advance)))
    (dotimes (idx (length props))
      (when (eq (logand idx 1) 0)
        (let* ((prop-name (nth idx props))
               (val (plist-get props prop-name)))
          (overlay-put overlay prop-name val))))
    overlay))

(defun describe-syntax-ppss--unset-and-remove (var-symbol)
  "Remove overlay from VAR-SYMBOL value."
  (when (overlayp (symbol-value var-symbol))
    (delete-overlay (symbol-value var-symbol)))
  (set var-symbol nil))

(defun describe-syntax-ppss--overlay-set (var-symbol start end &optional buffer
                                                     front-advance rear-advance
                                                     &rest props)
  "Create a new overlay and set value of VAR-SYMBOL to it.
If omitted, BUFFER defaults to the current buffer.
START and END may be integers or markers.
The fourth arg FRONT-ADVANCE, if non-nil, makes the marker
for the front of the overlay advance when text is inserted there
\(which means the text *is not* included in the overlay).
The fifth arg REAR-ADVANCE, if non-nil, makes the marker
for the rear of the overlay advance when text is inserted there
\(which means the text *is* included in the overlay).
PROPS is a plist to put on overlay."
  (when (overlayp (symbol-value var-symbol))
    (delete-overlay (symbol-value var-symbol)))
  (set var-symbol nil)
  (set var-symbol (apply #'describe-syntax-ppss--overlay-make
                         (append (list start end
                                       buffer front-advance
                                       rear-advance)
                                 props))))

(defun describe-syntax-ppss-update-overlays (positions)
  "Update overlays with syntax information at POSITIONS.

Argument POSITIONS is a list of buffer positions where overlays should be
updated."
  (require 'color)
  (let ((ovs)
        (curr-pos (point))
        (cl (foreground-color-at-point))
        (bg (background-color-at-point)))
    (dotimes (i (length positions))
      (let ((pos (nth i positions))
            (next-pos (or (nth (1+ i) positions)
                          curr-pos))
            (percent (round (/ (* 100 i)
                               (length positions)))))
        (let ((pl `(:background
                    ,(color-saturate-name
                      cl
                      percent)
                    :foreground
                    ,(color-saturate-name
                      bg
                      percent)))
              (selected (= pos curr-pos)))
          (when selected
            (plist-put pl :weight 'extra-bold))
          (push
           (describe-syntax-ppss--overlay-make pos (1+ pos)
                                               nil
                                               nil
                                               nil
                                               'face
                                               '(:weight extra-bold)
                                               'describe-syntax--ppss t)
           ovs)
          (push
           (describe-syntax-ppss--overlay-make pos next-pos
                                               nil
                                               nil
                                               nil
                                               'face
                                               pl
                                               'describe-syntax--ppss t)
           ovs))))
    ovs))

(defun describe-syntax-ppss-visit-open-parens (positions)
  "Incrementally highlight lists at POSITIONS."
  (require 'color)
  (let ((ovs))
    (unwind-protect
        (progn
          (setq ovs (describe-syntax-ppss-update-overlays positions))
          (let
              ((res
                (describe-syntax-ppss--completing-read-with-preview
                 "Position: "
                 (mapcar (apply-partially #'format "%s") positions)
                 (lambda (it)
                   (let ((pos (string-to-number it)))
                     (dolist (ov ovs)
                       (and (overlayp ov)
                            (delete-overlay ov)))
                     (save-excursion
                       (goto-char pos)
                       (setq ovs (describe-syntax-ppss-update-overlays positions))))))))
            (goto-char (string-to-number res))))
      (dolist (ov ovs)
        (and (overlayp ov)
             (delete-overlay ov))))))

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
