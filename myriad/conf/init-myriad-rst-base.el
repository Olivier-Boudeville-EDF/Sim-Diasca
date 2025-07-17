;; This is our configuration of Emacs regarding RST mode (ReStructuredText).
;;
;; This major mode is shown as "ReST".
;;
;; Refer to https://docutils.sourceforge.io/docs/user/emacs.html


(provide 'init-myriad-rst-base)

;; No rst.el package needed (rst-mode is built-in).

;; Wanting to use rst-mode in files with the following extensions:
(setq auto-mode-alist
	  (append '(("\\.txt$"  . rst-mode)
				("\\.rst$"  . rst-mode)
				("\\.rst.template$"  . rst-mode)
				("\\.rest$" . rst-mode)) auto-mode-alist))


;; Automatically update the table of contents everytime a section title is
;; modified:
;;
(add-hook 'rst-adjust-hook 'rst-toc-update)


;; Corresponds to our conventions in demo-for-css-testing.rst:
;;
;; (not correctly applied apparently, though)
;;
(setq rst-preferred-adornments'( (?= over-and-under 0)
				 (?- over-and-under 0)
				 (?= simple 0)
				 (?- simple 0)
				 (?. simple 0)
				 (?_ simple 0)
				 (?* simple 0)
				 (?: simple 0)
				 (?+ simple 0) ))
