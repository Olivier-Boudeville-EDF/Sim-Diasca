;; This is a minimal configuration of Emacs regarding C/C++.


;; Indentation:
;; Starting from its second line, a multi-line statement should be
;; indented of 2 characters from the beginning of line, not relatively
;; to, say, the opening parenthesis which can be close to the right edge
;; of the line.
;;
(setq c-offsets-alist '(
		;; Otherwise parameters are aligned with the first, whereas we want a
		;; fixed offset:
		(arglist-cont-nonempty . 2)
		(arglist-intro . 2)))


;; Support for C-like languages:
;; (customizations for all of c-mode, c++-mode, objc-mode, java-mode)
(defun my-c-mode-common-hook ()
  (setq cc-default-style "bsd")
  (c-set-offset 'substatement-open 0))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'cc-mode-common-hook 'my-c-mode-common-hook)

;; So that it can be loaded with 'require':
(provide 'init-myriad-c-cpp-base)
