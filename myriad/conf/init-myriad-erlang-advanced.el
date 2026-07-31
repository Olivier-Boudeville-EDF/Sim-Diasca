;; This is a more advanced configuration of Emacs regarding Erlang than
;; init-myriad-erlang-base.el.
;;
;; Based on LSP.
;; For Erlang, used to rely on erlang_ls, now on elp.
;;
;; Refer to https://howtos.esperide.org/Erlang.html#using-emacs-for-erlang for
;; guidelines.



;; In a nutshell: possibly due to erlang_ls, quite a lot of these advanced
;; features did not seem ready for actual use, at least in our context; they
;; lag, use huge CPU resources, trigger numerous runtime errors ("LSP :: Method
;; not implemented: textDocument/documentSymbol" and all - origin being
;; erlang_ls), report essentially spurious errors (e.g. short of finding proper
;; includes), lack documentation, trigger unsollicited, problematic rebar3-based
;; rebuilds, freeze randomly and do not provide veritably interesting
;; information. For projects relying a lot on parse-transforms, they seem
;; borderly unusable.
;;
;; So we finally selected only the relevant features, which are currently
;; auto-completion and display documentation of element at mouse cursor.
;;
;; Note that this file covers advanded Erlang features, but most of them apply
;; to other languages as well.

(message "Entering Myriad Erlang-advanced configuration...")



;; YAML suport.

(add-hook 'yaml-mode-hook 'display-line-numbers-mode)

;; Not wanting clumsy attempt of spellchecking there:
(remove-hook 'yaml-mode-hook (lambda () (flyspell-mode 1)))


;; Surprisingly not built-in:
;;
;; With treesitter: yaml-ts-mode, yet requires an extra library for this
;; specific ts support.
;;
(use-package yaml-mode :ensure (:wait t) :demand t)

;; Useless: (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))



;; Require and enable the Yasnippet templating system.
;;
;; See https://wikemacs.org/wiki/Yasnippet.
;;
;; (used for example by company to define correctly auto-filled argument names
;; when the beginning of a function call has been typed)
;;
(use-package yasnippet :ensure (:wait t) :demand t)
(yas-global-mode t)




;; LSP Package.
;;
;; Refer to https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/ for
;; a proper (general-purpose) lsp-mode configuration.


;; Diagnostics providers:

;; As flycheck (from init-myriad-fully-integrated.el)
;; should most probably be disabled when using lsp:
;;
;;(remove-hook 'erlang-mode-hook 'flycheck-mode)


;; Same applies to flymake, which reports many spurious issues in our use case
;; (e.g. due to parse-transforms).
;;
;; Yet this has no effect:
;;(remove-hook 'erlang-mode-hook 'flymake-mode)

;; Same:
;;(flymake-mode -1)

;; The way of silencing them all (fly*), otherwise elp is eclipsed:
(setq lsp-diagnostics-provider :none)


(use-package lsp-mode

  ;; Allows avoiding many 'xxx loaded before Elpaca activation' warnings:
  :defer

  :config
  ;; Enable LSP automatically for Erlang files:
  ;;:hook ((erlang-mode . lsp)) ;; Not working properly
  (add-hook 'erlang-mode-hook #'lsp)

  ;; ELP, added as priority 0 (> -1) so takes priority over the built-in one:
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("elp" "server"))
                    :major-modes '(erlang-mode)
                    :priority 0
                    :server-id 'elp-for-myriad))
)


;; Supposed to prevent company from pre-filling arguments of a function
;; (e.g. transforming automatically 'f(' into 'f(${1:Message}), ${2:Count})'),
;; as, at least currently, the cursor is then wrongly placed at the end of the
;; line, resulting in an even less convenient system as the one without company
;; - however this setting has no effect, wherever it is put in this
;; configuration file, so it is currently commented-out:
;;
;; Actually the YAS templating system must be active for this auto-fill to work
;; correctly.
;;
;;(setq lsp-enable-snippet nil)


;; Refer to https://emacs-lsp.github.io/lsp-mode/page/lsp-erlang-elp/:

;; Still unable to display the doc of remote, exported types when hovering
;; on them (whereas it works for corresponding functions...).

;; When the overall layer is sufficiently correct:
;;(setq lsp-elp-eqwalizer-all t)

(setq lsp-elp-hover-actions-enable t)
(setq lsp-elp-hover-actions-doc-links-enable t)

;;(setq lsp-elp-lens-enable t)
;;(setq lsp-elp-lens-debug-enable t)
;;(setq lsp-elp-lens-links-enable t)
;;(setq lsp-elp-lens-run-coverage-enable t)
;;(setq lsp-elp-lens-run-enable t)
;;(setq lsp-elp-lens-run-interactive-enable t)

;;(setq lsp-elp-signature-help-enable nil)
(setq lsp-elp-signature-help-enable t)

(setq lsp-elp-types-on-hover-enable t)
(setq lsp-erlang-elp-types-on-hover t)


;; WARNING, for testing only:
;;(setq lsp-auto-configure t)

;;(setq lsp-eldoc-enable-hover t)
;;(setq lsp-eldoc-render-all t)

;; Displayed at the top of a frame (e.g. "src > utils > foo.erl"):
(setq lsp-headerline-breadcrumb-enable t)



;; Not wanting to be prompted again and again about installing bash-ls and thus
;; npm:
;;
(setq lsp-enable-suggest-server-download nil)


;; Code lenses:

;; The extra text annotating our code source (e.g. "Used XXX times") is
;; displayed is a code lens (here "function-references"; see
;; https://erlang-ls.github.io/articles/tutorial-code-lenses/).
;;
;; We prefer having these "function-references" disabled, this can be set in
;; myriad/conf/erlang_ls.config, to which ~/.config/erlang_ls is supposed to
;; point (as a symbolic link).
;;
;; Another deactivation option is to use here:
(setq lsp-lens-enable nil)



;; Enable LSP for Erlang files.
;;
;; Was disabled due too many usability concerns; notably, type checking is
;; based on the sources rather than on the generated BEAM files, resulting in a
;; vast number of false positives.
;;
;; Now LSP is very selectively enabled, mostly to:
;;
;; - enjoy, when leaving the mouse cursor on an element, the display of a
;; contextual window describing its type, documentation, etc.
;;
;; - enable auto-completion based not only an Erlang/OTP modules, but also user
;; ones


;; More general, even too much (e.g. will attempt to apply to
;; makefile-gmake-mod, emacs-lisp-mode, etc. and will fail):
;;
;;(add-hook 'prog-mode-hook #'lsp)


;; So:
(add-hook 'erlang-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'java-mode-hook   #'lsp)
(add-hook 'sh-mode-hook     #'lsp)


;;(with-eval-after-load 'lsp-mode
;; (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))


;; Customize prefix for key-bindings:
;; (would clash with "Go to line")
;;(setq lsp-keymap-prefix "C-l")

;; To trigger "Go to Definition", hit Super-key then d (for "Definition"); the
;; default binding is "C-c l g g":
(global-set-key (kbd "s-d") 'lsp-find-definition)

;; To trigger "Go to References", hit Super-key then r (for "References"); the
;; default binding is "C-c l g r":
(global-set-key (kbd "s-r") 'lsp-find-references)

;; To rename as a whole a symbol, hit Super-key then n (for "reName"); the
;; default binding is "C-c l r r":
(global-set-key (kbd "s-n") 'lsp-rename)



;; Activation of the logging for lsp-mode:
;;(setq lsp-log-io t)
(setq lsp-log-io nil)


;; No effect:
;;(setq lsp-completion-enable t)




;; LSP UI Package.
;;
;; For fancy sideline, popup documentation, VS Code-like peek UI, etc.
;;
;; See https://github.com/emacs-lsp/lsp-ui and
;; https://emacs-lsp.github.io/lsp-ui/
;;
;; Provides "on hover dialogs": when leaving the mouse cursor on an element,
;; displays a contextual window describing its type, documentation, etc.
;;

;; With a dark theme, preferring a lightgrey background color for lsp-ui-doc
;; windows (and, for a light theme, a darkgrey background):
;;
;; (must be defined *before* using lsp-ui)
(defface lsp-ui-doc-background
  '((((background light)) :background "#777777")
	(t :background "#dddddd"))
  "Background color of the documentation. Only the `background' is used in
this face."
  :group 'lsp-ui-doc)


;;(use-package lsp-ui
;;  :after lsp-mode
;;  :commands lsp-ui-mode)
(use-package lsp-ui
  :ensure (:wait t) :demand t)


;; Regarding sideline (shows on the current line, generally on the right,
;; information about the symbols, flycheck diagnostics and LSP code actions; at
;; least often displays useless information in our context).

;; Way too much gibberish content:
;;(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-enable nil)

(setq lsp-ui-sideline-show-diagnostics t)

;;(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-sideline-show-hover nil)

;;(setq lsp-ui-sideline-show-code-actions t)
(setq lsp-ui-sideline-show-code-actions nil)

;;(setq lsp-ui-sideline-update-mode 'line)
(setq lsp-ui-sideline-delay 0)

;;(setq lsp-ui-sideline-diagnostic-max-lines 20)


;; Regarding peek (to determine which code calls a given function):
(setq lsp-ui-peek-enable t)
(setq lsp-ui-peek-show-directory t)



;; Regarding lsp-ui-doc:

(setq lsp-ui-doc-enable t)
;;(setq lsp-ui-doc-enable nil)

;; Less interfering:
(setq lsp-ui-doc-position 'top)
;;(setq lsp-ui-doc-position 'bottom)
;;(setq lsp-ui-doc-position 'at-point)

;; Less crowded:
;;(setq lsp-ui-doc-side 'right)

;; Not taken for a notification here:
(setq lsp-ui-doc-side 'left)


;; Number of seconds before showing the doc:
(setq lsp-ui-doc-delay 0.2)

;; Emacs (keyboard) cursor disabled, generates some noise:
;;
(setq lsp-ui-doc-show-with-cursor t)
;;(setq lsp-ui-doc-show-with-cursor nil)

;; Mouse cursor a bit random (appears at mouse position, not in a customisable
;; direction):
;;
(setq lsp-ui-doc-show-with-mouse t)
;;(setq lsp-ui-doc-show-with-mouse nil)


;; lsp-ui-doc-background previously set above.

;;(setq lsp-ui-doc-border "orange")
(setq lsp-ui-doc-border "white")


;; There are lsp-ui-imenu options as well.

;; Too invasive:
;;(setq lsp-modeline-diagnostics-enable t)
(setq lsp-modeline-diagnostics-enable nil)


;; Same (doc of functions displayed there):
;;(setq lsp-modeline-code-actions-enable t)
(setq lsp-modeline-code-actions-enable nil)



;; lsp-signature-* disabled, as may interact wrongly with company auto-fill
;; (unsure):

;;(setq lsp-signature-auto-activate t)
(setq lsp-signature-auto-activate nil)

;;(setq lsp-signature-render-documentation t)
(setq lsp-signature-render-documentation nil)



;; LSP Origami Mode (for folding ranges):
;; (does not seem to apply to Erlang)
;;
;; Moreover triggers 'Package cl is deprecated'...
;;
;;(use-package lsp-origami :ensure (:wait t) :demand t)

;;(add-hook 'origami-mode-hook #'lsp-origami-mode)
;;(add-hook 'erlang-mode-hook #'origami-mode)


;; Provide commands to list workspace symbols:
;; - helm-lsp-workspace-symbol
;; - helm-lsp-global-workspace-symbol
(use-package helm-lsp :ensure (:wait t) :demand t)

(use-package lsp-treemacs :ensure (:wait t) :demand t)

;; Always show diagnostics at the bottom, using 1/3 of the available space:
;; (deactivated, as suspected of add visual noise/expanding an additional bottom
;; buffer)
;;
;; (add-to-list 'display-buffer-alist
;;			 `(,(rx bos "*Flycheck errors*" eos)
;;			  (display-buffer-reuse-window
;;			   display-buffer-in-side-window)
;;			  (side            . bottom)
;;			  (reusable-frames . visible)
;;			  (window-height   . 0.33)))






;; company-mode section, for auto-completion popups.
;;
;; See https://company-mode.github.io/ and https://wikemacs.org/wiki/Company-mode
;;
;; Can be deactivated with '(setq lsp-completion-provider :none)'.


;; Enable company by default, except for texts:


;; This form does not allow to set/unset easily company based on mode and, more
;; generally, seems at least fragile if not dysfunctioning:
;;
;; (use-package company
;;   :after lsp-mode
;;   ;; Does not work: ':hook prog-mode' ('company' not found)
;;   ;;                ':hook (prog-mode . company-mode)'
;;   :bind (:map company-active-map
;;          ("<tab>" . company-complete-selection))
;;         (:map lsp-mode-map
;;          ("<tab>" . company-indent-or-complete-common))
;;   :custom
;;   (company-minimum-prefix-length 1)
;;   ;; Default is 0.2:
;;   (company-idle-delay 0.0))

;;(use-package company :ensure (:wait t) :demand t)

;; Does not work: '(add-hook 'after-init-hook 'global-company-mode)'
(add-hook 'prog-mode-hook 'company-mode)

(setq company-minimum-prefix-length 0)

(setq company-idle-delay
      (lambda () (if (company-in-string-or-comment) nil 0.2)))

(custom-set-faces
;; ;; Rather light grey to be readable / rather blue:
 '(company-tooltip ((t (:background "#dddddd" :foreground "#050566")))))





;; Not working properly either ('No such file or directory, company-box'):
;;(use-package company-box
;; :hook (company-mode . company-box-mode))


;; Supposedly a tad better:
(use-package company-box :ensure (:wait t) :demand t)
(add-hook 'company-mode-hook 'company-box-mode)



;; So that it can be loaded with 'require':
(provide 'init-myriad-erlang-advanced)
