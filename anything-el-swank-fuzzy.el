;;; anything-el-swank-fuzzy.el --- anything-sources for el-swank-fuzzy.el

;; Copyright (C) 2009 Takeshi Banse <takebi@laafc.net>
;; Author: Takeshi Banse <takebi@laafc.net>
;; Keywords: matching, convenience, anything

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Some Anything configurations for using the el-swank-fuzzy.el.

;;; Installation:
;;
;; Put the el-swank-fuzzy.el, anything.el and anything-complete.el to your
;; load-path.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-el-swank-fuzzy-complete-functions'
;;    `lisp-complete-symbol' for functions using `anything'.
;;  `anything-el-swank-fuzzy-complete-variables'
;;    `lisp-complete-symbol' for variables using `anything'.
;;  `anything-el-swank-fuzzy-complete-symbol'
;;    `lisp-complete-symbol' using `anything'.
;;  `anything-el-swank-fuzzy-indent-and-complete-symbol'
;;    Indent the current line and perform `anything-el-swank-fuzzy-complete-symbol'.
;;  `anything-el-swank-fuzzy-indent-and-complete-functions'
;;    Indent the current line and perform `anything-el-swank-fuzzy-complete-functions'.
;;  `anything-el-swank-fuzzy-indent-and-complete-variables'
;;    Indent the current line and perform `anything-el-swank-fuzzy-complete-variables'.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `anything-el-swank-fuzzy-completions-prefix-length'
;;    *Number of the prefix length which is the length of substring to which the prefix match should be performed in `el-swank-fuzzy-completions'.
;;    default = 2
;;  `anything-el-swank-fuzzy-completions-time-in-msec'
;;    *Number of the time limit spent (in msec) while gathering completions in `el-swank-fuzzy-completions'.
;;    default = 1500
;;  `anything-el-swank-fuzzy-complete-symbol-classify'
;;    *If non-nil, use separate source for the functions/variables in `anything-el-swank-fuzzy-complete-symbol'.
;;    default = t

;;; History:

;; v0.2.1
;; Fix "No buffer named *anything complete*" bug.
;; Thanks deftsp for the report.

;; v0.2.0
;; * Add `anything-el-swank-fuzzy-complete-symbol'.
;; * Add `anything-el-swank-fuzzy-indent-and-complete-symbol' respectively.
;; Thanks deftsp for the suggestion.

;; v0.1.0
;; Initial version.

;;; Code:

(require 'anything)
(require 'anything-complete)
(require 'cl)
(require 'el-swank-fuzzy)
(require 'eldoc)

(when (require 'anything-show-completion nil t)
  (dolist (f '(anything-el-swank-fuzzy-complete-functions
               anything-el-swank-fuzzy-complete-variables
               anything-el-swank-fuzzy-complete-symbol))
    (use-anything-show-completion f '(length anything-complete-target))))

(defcustom anything-el-swank-fuzzy-completions-prefix-length 2
  "*Number of the prefix length which is the length of substring to which the prefix match should be performed in `el-swank-fuzzy-completions'."
  :type 'integer
  :group 'anything-complete)
(defcustom anything-el-swank-fuzzy-completions-time-in-msec 1500
  "*Number of the time limit spent (in msec) while gathering completions in `el-swank-fuzzy-completions'."
  :type 'integer
  :group 'anything-complete)

;; Copied from the anything-complete.el and added an optional parameter
;; TARGET-DEFAULT-INPUT-P to not defaulting the target for some kind of
;; the compound/fuzzy completes.
(defun aeswf-anything-noresume (&optional any-sources any-input any-prompt any-resume any-preselect any-buffer)
  (let (anything-last-sources anything-compiled-sources anything-last-buffer)
    (anything any-sources any-input any-prompt any-resume any-preselect any-buffer)))
(defun aeswf-anything-complete (sources target &optional limit idle-delay input-idle-delay target-default-input-p)
  (let ((anything-candidate-number-limit (or limit anything-candidate-number-limit))
        (anything-idle-delay (or idle-delay anything-idle-delay))
        (anything-input-idle-delay (or input-idle-delay anything-input-idle-delay))
        (anything-complete-target target)
        (anything-execute-action-at-once-if-one t)
        (enable-recursive-minibuffers t)
        anything-samewindow)
    (aeswf-anything-noresume sources (if target-default-input-p target nil)
                             nil nil nil "*anything complete*")))

(defun aeswf-put-any-realvalue-property (real)
  (put-text-property (point-at-bol 0) (point-at-eol 0)
                     'anything-realvalue
                     real))
(defun* aeswf-init-candidates-buffer-base
    (complete insert &optional (put-text-property1 'aeswf-put-any-realvalue-property))
  (let* ((completion-result (with-current-buffer anything-current-buffer
                              (funcall complete)))
         (completions (first completion-result))
         (base  (second completion-result)))
    (with-current-buffer (anything-candidate-buffer 'global)
      (funcall insert completions base put-text-property1))))

;;; borrowed from swank-fuzzy.el and edited.
(defun aeswf-insert-completion-choice (completion &optional _max-length)
  "Inserts the completion object `completion' as a formatted
completion choice into the current buffer, and mark it with the
proper text properties."
  (destructuring-bind (symbol-name score chunks classification-string)
      completion
    (let ((start (point))
          (end))
      (insert symbol-name)
      (setq end (point))
      (dolist (chunk chunks)
        (put-text-property (+ start (first chunk)) 
                           (+ start (first chunk) (length (second chunk)))
                           'face 'bold))
      (insert "\n"))))

(defun* aeswf-init-candidates-buffer (pred &optional (prefix-length anything-el-swank-fuzzy-completions-prefix-length))
  (let ((complete
         (apply-partially 'el-swank-fuzzy-completions
                          anything-complete-target
                          anything-el-swank-fuzzy-completions-time-in-msec
                          pred
                          prefix-length)))
    (aeswf-init-candidates-buffer-base complete
                                       (lambda (completions _b put-property)
                                         (dolist (c completions)
                                           (aeswf-insert-completion-choice c)
                                           (funcall put-property (car c)))))))

;;; anything-show-completion.el extension.
(defun aeswf-transformer-prepend-spacer0 (prepend candidates source)
  (mapcar (lambda (cand)
            (cons (funcall prepend cand)
                  (or (get-text-property 0 'anything-realvalue cand) cand)))
          candidates))
(defun* aeswf-current-column (&optional (target anything-complete-target)
                                        (buffer anything-current-buffer))
  (with-current-buffer buffer
    (save-excursion
      (backward-char (string-width target))
      (let ((col (alcs-current-physical-column)))
        (if (< col 0) ;; XXX: WTF? Fall off?
            (- (point) (save-excursion (vertical-motion -1) (point)))
          col)))))

(require 'term) ;; term-window-width
(defalias 'aeswf-window-width 'term-window-width)
(defvar aeswf-transformer-prepend-spacer-saved-column nil)
(defun* aeswf-transformer-prepend-spacer-compute
    (candidates source &optional (compute (lambda (col sw ww)
                                            (- col (- (+ col sw) ww)))))
  (flet ((save-column-maybe ()
           (or aeswf-transformer-prepend-spacer-saved-column
               (setq aeswf-transformer-prepend-spacer-saved-column
                     (let ((ww (aeswf-window-width))
                           (sw (reduce (lambda (acc x)
                                         (max acc (string-width x)))
                                       candidates
                                       :initial-value 0))
                           (col (aeswf-current-column)))
                       (if (< (+ col sw) ww)
                           col
                         (funcall compute col sw ww)))))))
    (if candidates
        (let ((column (save-column-maybe)))
          (aeswf-transformer-prepend-spacer0 (lambda (cand)
                                               (concat (make-string column ? )
                                                       cand))
                                               candidates
                                               source))
      candidates)))
(defun aeswf-transformer-prepend-spacer-saved-column-clear ()
  (setq aeswf-transformer-prepend-spacer-saved-column nil))
(when (and (boundp 'anything-show-completion-activate))
  (add-hook 'anything-cleanup-hook 'aeswf-transformer-prepend-spacer-saved-column-clear))

(defun aeswf-transformer-prepend-spacer-maybe (candidates source)
  (if (and (boundp 'anything-show-completion-activate)
           anything-show-completion-activate)
      (let ((compute (lambda (col sw ww)
                       (- ww sw))))
        (aeswf-transformer-prepend-spacer-compute candidates source compute))
    candidates))

;;; basic completion.
(defvar anything-el-swank-fuzzy-complete-functions
  `((name . "el-swank-fuzzy functions")
    (init . ,(apply-partially 'aeswf-init-candidates-buffer 'fboundp))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (type . complete-function)
    (filtered-candidate-transformer aeswf-transformer-prepend-spacer-maybe)))
(defvar anything-el-swank-fuzzy-complete-variables
  `((name . "el-swank-fuzzy variables")
    (init . ,(apply-partially 'aeswf-init-candidates-buffer 'boundp))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (type . complete-variable)
    (filtered-candidate-transformer aeswf-transformer-prepend-spacer-maybe)))
(defun aeswf-complete (sources)
  (flet ((get-input ()
           (let ((b (bounds-of-thing-at-point 'symbol)))
             (when b
               (buffer-substring-no-properties (point)
                                               (save-excursion
                                                 (goto-char (car b))
                                                 (point)))))))
    (let ((input (get-input)))
      (if (and input
               (< anything-el-swank-fuzzy-completions-prefix-length
                  (length input)))
          (aeswf-anything-complete sources input)
        (message (format "%s: at least %d chars needed:\"%s\""
                         this-command
                         (1+ anything-el-swank-fuzzy-completions-prefix-length)
                         input))))))

(defvar anything-el-swank-fuzzy-complete-functions-sources
  '(anything-el-swank-fuzzy-complete-functions))
(defun anything-el-swank-fuzzy-complete-functions ()
  "`lisp-complete-symbol' for functions using `anything'."
  (interactive)
  (aeswf-complete anything-el-swank-fuzzy-complete-functions-sources))

(defvar anything-el-swank-fuzzy-complete-variables-sources
  '(anything-el-swank-fuzzy-complete-variables))
(defun anything-el-swank-fuzzy-complete-variables ()
  "`lisp-complete-symbol' for variables using `anything'."
  (interactive)
  (aeswf-complete anything-el-swank-fuzzy-complete-variables-sources))

;;; hybrid completion.
(defun aeswf-complete-symbol-meta-source-init ()
  (flet ((init-candidate-buffer (name)
           (let ((anything-source-name (format "el-swank-fuzzy %s" name)))
             (anything-candidate-buffer 'global)))
         (insert-candidate (buffer completion)
           (with-current-buffer buffer
             (aeswf-insert-completion-choice completion)
             (aeswf-put-any-realvalue-property (car completion)))))
    (multiple-value-bind (completions _interrupted-p)
        (el-swank-fuzzy-completions
         anything-complete-target
         anything-el-swank-fuzzy-completions-time-in-msec
         (lambda (s) (or (boundp s) (fboundp s)))
         anything-el-swank-fuzzy-completions-prefix-length)
      (loop for c in completions
            for sym = (intern (car c))
            with fbuffer = (init-candidate-buffer "functions")
            with bbuffer = (init-candidate-buffer "variables")
            when (fboundp sym)
            do (insert-candidate fbuffer c)
            when (boundp sym)
            do (insert-candidate bbuffer c)
            finally do
            (flet ((source (name type)
                     `((name . ,(format "el-swank-fuzzy %s" name))
                       (candidates-in-buffer)
                       (get-line . buffer-substring)
                       (type . ,type)
                       (filtered-candidate-transformer
                        aeswf-transformer-prepend-spacer-maybe))))
              (anything-set-sources (list
                                     (source "functions" 'complete-function)
                                     (source "variables" 'complete-variable))
                                    nil t))))))

(defcustom anything-el-swank-fuzzy-complete-symbol-classify t
  "*If non-nil, use separate source for the functions/variables in `anything-el-swank-fuzzy-complete-symbol'."
  :type 'boolean
  :group 'anything-complete)
(defun aeswf-complete-symbol-source (classifyp)
  (if classifyp
      '(((name . "el-swank-fuzzy symbol meta source")
         (init . aeswf-complete-symbol-meta-source-init)))
    `(((name . "el-swank-fuzzy symbol")
       (init . ,(apply-partially 'aeswf-init-candidates-buffer
                                 (lambda (s) (or (boundp s) (fboundp s)))))
       (get-line . buffer-substring)
       (candidates-in-buffer)
       (type . complete)))))
(defun anything-el-swank-fuzzy-complete-symbol ()
  "`lisp-complete-symbol' using `anything'."
  (interactive)
  (aeswf-complete (aeswf-complete-symbol-source
                   anything-el-swank-fuzzy-complete-symbol-classify)))

;;; indent-and-complete.
(defun aeswf-echo-arglist ()
  ;; This makes forcibly update the minibuffer contents for me.
  (letf (((symbol-function 'eldoc-display-message-p)
          (lambda () eldoc-mode)))
    (eldoc-print-current-symbol-info)))
(defun* aeswf-indent-and-funcall
    (function &optional (indent-line 'lisp-indent-line) (echo-arglist 'aeswf-echo-arglist))
  ;;; borrowed from slime.el.
  (let ((pos (point)))
    (funcall indent-line)
    (when (= pos (point))
      (cond ((save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
             (funcall function))
            ((memq (char-before) '(?\t ?\ ))
             (funcall echo-arglist))))))

(defun anything-el-swank-fuzzy-indent-and-complete-symbol ()
  "Indent the current line and perform `anything-el-swank-fuzzy-complete-symbol'."
  (interactive)
  (aeswf-indent-and-funcall 'anything-el-swank-fuzzy-complete-symbol))
(defun anything-el-swank-fuzzy-indent-and-complete-functions ()
  "Indent the current line and perform `anything-el-swank-fuzzy-complete-functions'."
  (interactive)
  (aeswf-indent-and-funcall 'anything-el-swank-fuzzy-complete-functions))
(defun anything-el-swank-fuzzy-indent-and-complete-variables ()
  "Indent the current line and perform `anything-el-swank-fuzzy-complete-variables'."
  (interactive)
  (aeswf-indent-and-funcall 'anything-el-swank-fuzzy-complete-variables))

(provide 'anything-el-swank-fuzzy)
;;; anything-el-swank-fuzzy.el ends here
