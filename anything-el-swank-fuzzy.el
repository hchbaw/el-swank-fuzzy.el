;;; anything-el-swank-fuzzy.el --- anything-sources for el-swank-fuzzy.el

;; Copyright (C) 2009 Takeshi Banse <takebi@laafc.net>
;; Author: Takeshi Banse <takebi@laafc.net>
;; Keywords: matching, convenience, anything

;;; Code:

(require 'anything-complete)
(require 'el-swank-fuzzy)

(when (require 'anything-show-completion nil t)
  (dolist (f '(anything-el-swank-fuzzy-complete-functions
               anything-el-swank-fuzzy-complete-variables))
    (use-anything-show-completion f '(length anything-complete-target))))

;; Copied from the anything-complete.el and added an optional parameter
;; TARGET-IS-DEFAULT-INPUT-P to not defaulting the target for some kind of
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

(defun aeswf-init-candidates-buffer-base (complete insert)
  (let ((put-text-property1 (lambda (s)
                              (put-text-property (point-at-bol 0)
                                                 (point-at-eol 0)
                                                 'anything-realvalue
                                                 s))))
    (let* ((completion-result (with-current-buffer anything-current-buffer
                                (funcall complete)))
           (completions (first completion-result))
           (base  (second completion-result)))
      (with-current-buffer (anything-candidate-buffer 'global)
        (funcall insert completions base put-text-property1)))))

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

(defun aeswf-init-candidates-buffer (pred time-limie-in-msec)
  (aeswf-init-candidates-buffer-base (apply-partially 'el-swank-fuzzy-completions
                                                      anything-complete-target
                                                      time-limie-in-msec
                                                      pred)
                                     (lambda (completions _b put-property)
                                       (dolist (c completions)
                                         (aeswf-insert-completion-choice c)
                                         (funcall put-property (car c))))))

(defvar anything-el-swank-fuzzy-complete-functions
  `((name . "el-swank-fuzzy functions")
    (init . ,(apply-partially 'aeswf-init-candidates-buffer 'fboundp 1500))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (type . complete-function)))
(defvar anything-el-swank-fuzzy-complete-variables
  `((name . "el-swank-fuzzy variables")
    (init . ,(apply-partially 'aeswf-init-candidates-buffer 'boundp 1500))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (type . complete-variable)))
(defun aeswf-complete (sources)
  (aeswf-anything-complete sources
                           (buffer-substring-no-properties
                            (point)
                            (save-excursion
                              (let ((b (bounds-of-thing-at-point 'symbol)))
                                (goto-char (car b))
                                (point))))))

(defvar anything-el-swank-fuzzy-complete-functions-sources
  '(anything-el-swank-fuzzy-complete-functions))
(defun anything-el-swank-fuzzy-complete-functions ()
  (interactive)
  (aeswf-complete anything-el-swank-fuzzy-complete-functions-sources))

(defvar anything-el-swank-fuzzy-complete-variables-sources
  '(anything-el-swank-fuzzy-complete-variables))
(defun anything-el-swank-fuzzy-complete-variables ()
  (interactive)
  (aeswf-complete anything-el-swank-fuzzy-complete-variables-sources))

(provide 'anything-el-swank-fuzzy)
;;; anything-el-swank-fuzzy.el ends here
