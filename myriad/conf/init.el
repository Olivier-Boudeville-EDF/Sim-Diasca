;; This is the Ceylan-Myriad default Emacs configuration file, typically to
;; exist as ~/.emacs/init.el; check that none of the many other Emacs
;; configuration file gets in the way.


;;; package --- This is an initialization script written in elisp.

;;; Commentary: refer to
;;; https://www.gnu.org/software/emacs/manual/html_node/elisp/

;;; Code: (below)"

;; Section for package management with straight.el

;; straight.el is a replacement for package.el (not use-package).
;; use-package can be used with either package.el or straight.el.

;; Must be kept, in order to prevent package.el loading packages prior to their
;; init-file loading:

(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
	   (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	  (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
	(with-current-buffer
		(url-retrieve-synchronously
		 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
		 'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)



;; For proper mouse search:

;; Do not consider underscores and dashes as word separators (otherwise
;; mouse-based search changes its selection during search):
;;
;; (probably a bad idea, search patterns seem not to be found when having a
;; prefix)
;;
;;(global-superword-mode 1)


;; Not useful finally for mouse searches:
;;(straight-use-package 'helm)


;; See https://stackoverflow.com/questions/202803/searching-for-marked-selected-text-in-emacs:
(defun jrh-isearch-with-region ()
  "Use region as the isearch text."
  (interactive)
  (when mark-active
	(let ((region (funcall region-extract-function nil)))
	  (deactivate-mark)
	  (isearch-push-state)
	  (isearch-yank-string region))))

(add-hook 'isearch-mode-hook #'jrh-isearch-with-region)

(progn

  ;; set arrow keys in isearch. left/right is backward/forward, up/down is
  ;; history. press Return to exit

  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )

  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)

  (define-key isearch-mode-map [(mouse-3)] 'isearch-repeat-forward)

  ;; Only drawback: as the mark is at the end of the expression, we have to
  ;; request each previous occurrence twice:
  ;;
  (define-key isearch-mode-map [(shift mouse-3)] 'isearch-repeat-backward)

  (define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-reverse-exit-minibuffer)
  (define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-forward-exit-minibuffer))


;;(require 'acme-search)
;;(global-set-key [(mouse-3)] #'acme-search-forward)
;;(global-set-key [(shift mouse-3)] #'acme-search-backward)


;;(global-set-key [(mouse-3)] #'jrh-isearch-with-region)
(global-set-key [(mouse-3)] #'isearch-forward)

;;(global-set-key [(shift mouse-3)] #'acme-search-backward)
(global-set-key [(shift mouse-3)] #'isearch-backward)



;; Set foreground and background colors:

(set-foreground-color "white")
(set-background-color "black")

;;; Set highlighting colors for isearch and drag
;;(set-face-foreground 'highlight "white")

;; Color for the cursor line:
(set-face-background 'highlight "gray19")
;;(set-face-background 'highlight "black")

(set-face-foreground 'region "black")
(set-face-background 'region "lightgreen")

;;(set-face-foreground 'secondary-selection "skyblue")
;;(set-face-background 'secondary-selection "darkblue")
(set-face-foreground 'secondary-selection "red")
(set-face-background 'secondary-selection "green")

;; Turns off a blinking cursor:
(if (fboundp 'blink-cursor-mode)
	(blink-cursor-mode -1))

;;(setq frame-background-mode 'dark)
'(frame-background-mode (quote dark))

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)
(transient-mark-mode t)

(setq use-file-dialog nil)

;;(standard-display-european 1)

(setq vc-follow-symlinks nil)
(setq line-move-visual nil)


;; Moves the cursor across "physical lines":
;; (finally deactivated, as the 'go to end of line' key was leading to the
;; cursor going downward...)
;;(require 'physical-line)
;;(add-hook 'find-file-hooks 'physical-line-mode-without-exception)


;; Only for older Emacs apparently:(setq default-tab-width 4)
(setq-default tab-width 4)

(setq tab-width 4)

(setq scroll-step 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(transient-mark-mode 1)
(savehist-mode 1)

(setq frame-title-format '("%b" (buffer-file-name ": %f")))


(server-start)

;; No more question about clients being still there:
;; (must be *after* server-start)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)


(setq tags-table-list '("~/.ctags.d"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 ;;'(package-selected-packages '(flycheck yasnippet which-key lsp-origami helm-lsp erlang))
 '(package-selected-packages '(flycheck which-key erlang))
 '(show-paren-mode t)
 ;; Not wanting this mostly useless graphical toolbar at the top:
 '(tool-bar-mode nil)
 '(warning-suppress-types '((server))))

;; :height 95 for some resolutions:
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(rst-level-1-face ((t (:background "#00f" :foreground "#fff"))) t)
 '(rst-level-2-face ((t (:background "#00a" :foreground "#ddd"))) t)
 '(rst-level-3-face ((t (:background "#003" :foreground "#bbb"))) t)
 '(rst-level-4-face ((t (:background "#000" :foreground "#999"))) t)
 '(rst-level-5-face ((t (:background "#010" :foreground "#666"))) t)
 '(rst-level-6-face ((t (:background "#020" :foreground "#555"))) t))


 ;; No more annoying, unmutable bell (e.g. when reaching buffer bounds):
(setq ring-bell-function 'ignore)

(straight-use-package 'server)

(straight-use-package 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; Flycheck is interesting yet confused by the Myriad, WOOPER, etc. parse
;; transforms:
;;
(setq flycheck-global-modes '(not erlang-mode))


;; RST files support section.

;; May be disabled if slowing emacs down way too much:
(straight-use-package 'rst)
(setq auto-mode-alist
	  (append '(("\\.txt$"  . rst-mode)
				("\\.rst$"  . rst-mode)
				("\\.rst.template$"  . rst-mode)
				("\\.rest$" . rst-mode)) auto-mode-alist))


;; Automatically update the table of contents everytime you adjust a
;; section title:
(add-hook 'rst-adjust-hook 'rst-toc-update)


;; Corresponds to conventions in demo-for-css-testing.rst:
;; (not correctly applied apparently, though)
(setq rst-preferred-adornments'( (?= over-and-under 0)
								 (?- over-and-under 0)
								 (?= simple 0)
								 (?- simple 0)
								 (?. simple 0)
								 (?_ simple 0)
								 (?* simple 0)
								 (?: simple 0)
								 (?+ simple 0) ))




(defun fix-behaviours-for-text-modes ()
  ;;(message "############## Fixing behaviours for text modes ###########")

  ;; Advanced automatic indentation not adapted to text modes:
  (remove-hook 'find-file-hooks 'set-advanced-ret-behaviour)

  ;; This basic indentation is fine with text modes:
  (global-set-key (kbd "RET") 'newline-and-indent)

  ;;Long lines are normal in text modes:
  ;;(remove-hook 'find-file-hook 'highlight-80+-mode)
  ;; Surely an hack, but works great:
  ;;(setq-local whitespace-line-column 9999)

  ;; No 'lines' or 'empty':
  (setq-local whitespace-style '(face
	tabs trailing space-before-tab newline
	indentation space-after-tab))
  )

(add-hook 'text-mode-hook 'fix-behaviours-for-text-modes)


;; Erlang support:

;; Adapted from the README distributed with the OTP tarballs:

;; Note: 'emacs' is here a symbolic link typically created by our
;; install-erlang.sh script, so that upgrading Erlang does not risk to make this
;; directory (e.g. lib/tools-2.8.2/emacs) vanish because of a change in the
;; 'tools' version (thus requiring the current file to be endlessly modified)

;; Two possible conventional locations for an Erlang install:
;;  - either in the user account (in ~/Software/Erlang/Erlang-current-install)
;;  - or directly in the system tree (in /usr/local/lib/erlang/)

(setq load-path (cons "~/Software/Erlang/Erlang-current-install/lib/erlang/emacs" load-path))
(setq load-path (cons "/usr/local/lib/erlang/emacs" load-path))

;;(setq erlang-root-dir "~/Software/Erlang/Erlang-current-install/lib/erlang")
;;(setq erlang-root-dir "/usr/local/lib/erlang")

(setq exec-path (cons "~/Software/Erlang/Erlang-current-install/lib/erlang/bin" exec-path))
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))


;; Allows to have Emacs automatically insert newlines to word-wrap:
;; (see https://www.emacswiki.org/emacs/AutoFillMode)
;;
;; See https://erlang.org/doc/apps/tools/erlang_mode_chapter.html
;;
;; Only in some language modes, not all text modes nor even all programming
;; modes where it is more of a nuisance:
;;
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;;(add-hook 'prog-mode-hook 'turn-on-auto-fill)
;; Not so useful even here:
;;(add-hook 'erlang-mode-hook 'turn-on-auto-fill)

;;(require 'erlang-start)

;; Not wanting single '%' to be set at the default column 48:
(add-hook 'erlang-mode-hook (lambda () (setq-local comment-column 0)))

;; erlang-electric-semicolon removed, as more a nuisance than a help (function
;; headers generally pasted from first):
;;
;;(setq erlang-electric-commands '(erlang-electric-comma
;;								 erlang-electric-g))

;; Disable all electric commands:
(setq erlang-electric-commands '())

(setq auto-mode-alist
	  (append '(("\\.escript$" . erlang-mode)) auto-mode-alist))


(straight-use-package 'erlang)



;; Indentation:

; Automatic indentation while typing:

;; Does not work correctly with inner bullet lists:
;;(setq indent-line-function 'indent-relative-maybe)

;; Just indents by default at the same level when Enter is hit:
;;(add-hook 'find-file-hooks '(lambda ()
;;      (local-set-key (kbd "RET") 'newline-and-indent)))


;; Useful for most programming modes, but disrupts sub-bullet lists in
;; text (e.g. RST) modes (puts them all at the same level):
;; (not defined as a lambda in order to be able to remove it)
(defun set-advanced-ret-behaviour ()
  ;;(message "############ Setting advanced RET behaviour ###########")
  ;;(local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
  (global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
  )

;;(add-hook 'find-file-hooks 'set-advanced-ret-behaviour)



;; Starting from its second line, a multi-line statement should be
;; indented of 2 characters from the beginning of line, not relatively
;; to, say, the opening parenthesis which can be close to the right edge
;; of the line.
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

;; Not working apparently with emacs 22.2.1:
;;(auto-raise-mode t)


;; Taken from https://anirudhsasikumar.net/blog/2005.01.21.html to protect
;; credentials (see our open-credentials.sh script):

(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Sensitive"
  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-mode)
	  (progn
	;; disable backups
	(set (make-local-variable 'backup-inhibited) t)
	;; disable auto-save
	(if auto-save-default
		(auto-save-mode -1)))
	;resort to default value of backup-inhibited
	(kill-local-variable 'backup-inhibited)
	;resort to default auto save setting
	(if auto-save-default
	(auto-save-mode 1))))

(setq auto-mode-alist
 (append '(("\\.swap.dat$" . sensitive-mode))
			   auto-mode-alist))


;; LSP-related section
;; See ~/Software/erlang_ls/misc/dotemacs for a configuration example.

;; Requires erlang_ls, typically obtained with:
;; cd ~/Software
;; git clone https://github.com/erlang-ls/
;; cd erlang_ls && make && mkdir bin && cd bin
;;   && ln -s ../_build/default/bin/erlang_ls
;; Then add ${HOME}/Software/erlang_ls/bin to your PATH.

;; Include the Language Server Protocol Clients:
;;(package-require 'lsp-mode)

;; Customize prefix for key-bindings:
;; (would clash with "Go to line")
;;(setq lsp-keymap-prefix "C-l")

;; Enable LSP for Erlang files:
;;
;; (disabled due too many usability concerns)
;;
;;(add-hook 'erlang-mode-hook #'lsp)

;; Require and enable the Yasnippet templating system:
;;(package-require 'yasnippet)
;;(yas-global-mode t)

;; Enable logging for lsp-mode:
;;(setq lsp-log-io t)
;;(setq lsp-log-io nil)

;; To select options, see:
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

;; Enable and configure the LSP UI Package:
;; See https://github.com/emacs-lsp/lsp-ui
;;(package-require 'lsp-ui)
;;(use-package lsp-ui)

;; Regarding sideline:
;;(setq lsp-ui-sideline-show-diagnostics t)
;;(setq lsp-ui-sideline-show-hover t)

;;(setq lsp-ui-sideline-show-code-actions t)
;;(setq lsp-ui-sideline-show-code-actions nil)

;;(setq lsp-ui-sideline-update-mode 'line)
;;(setq lsp-ui-sideline-delay ...

;;(setq lsp-ui-sideline-enable t)


;; Regarding peek:
;;(setq lsp-ui-peek-enable t)
;;(setq lsp-ui-peek-show-directory t)


;; Regarding lsp-ui-doc:
;;(setq lsp-ui-doc-enable t)
;;(setq lsp-ui-doc-position 'bottom)
;;(setq lsp-ui-doc-delay Number of seconds before showing the doc...
;;(setq lsp-ui-doc-show-with-cursor t)
;;(setq lsp-ui-doc-show-with-mouse t)


;; Enable LSP Origami Mode (for folding ranges):
;;(package-require 'lsp-origami)
;;(add-hook 'origami-mode-hook #'lsp-origami-mode)
;;(add-hook 'erlang-mode-hook #'origami-mode)

;; Provide commands for type completion, to list workspace symbols:
;; - helm-lsp-workspace-symbol
;; - helm-lsp-global-workspace-symbol
;;(package-install 'helm-lsp)

;;(add-hook 'after-init-hook 'global-company-mode)

;;(setq company-minimum-prefix-length 1
;;    company-idle-delay 0.0) ;; default is 0.2

;; Which-key integration:
;;(package-require 'which-key)
;;(add-hook 'erlang-mode-hook 'which-key-mode)

;;(with-eval-after-load 'lsp-mode
;; (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; Always show diagnostics at the bottom, using 1/3 of the available space:
;;(add-to-list 'display-buffer-alist
;;   `(,(rx bos "*LSP errors*" eos)
;;    (display-buffer-reuse-window
;;     display-buffer-in-side-window)
;;    (side            . bottom)
;;    (reusable-frames . visible)
;;    (window-height   . 0.33)))



;; Not used anymore, as faulty errors are triggered when using WOOPER:
;;(require 'erlang-flymake)


;; Section to show line and column numbers on the left border:
;; more info: https://www.emacswiki.org/emacs/LineNumbers

;; (obsolete now; see also 'longlines')
;;(require 'linum)
;;(add-hook 'find-file-hook (lambda () (linum-mode 1)))

;;(add-hook 'erlang-mode-hook 'linum-mode)
;;(add-hook 'erlang-mode-hook 'column-number-mode)

(straight-use-package 'display-line-numbers)


;; List of a major modes on which to disable line numbers:
(defcustom display-line-numbers-exempt-modes
  ;; Finally not including 'rst-mode' here, as useful for the debugging of
  ;; document generation:
  ;;
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode)
  "Major modes :."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
			  (member major-mode display-line-numbers-exempt-modes))
	(display-line-numbers-mode)))

(global-display-line-numbers-mode)


;; To be able to move between windows simply thanks to S-<arrow> (i.e. holding
;; shift, and hitting one of the 4 arrow keys; however the block selection
;; becomes then a bit more cumbersome):
;;
(windmove-default-keybindings)

;; No limit in the buffer list:
(setq buffers-menu-max-size nil)



;; Indenting buffers as a whole:
;; more info: https://www.emacswiki.org/emacs/DeletingWhitespace#h5o-11
(defun indent-whole-buffer ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
)

;; The find-file-hooks variable lists the functions to be called after a file is
;; visited (opened):
;;
;;(add-hook 'find-file-hook 'indent-whole-buffer)

;; Disabled, as was modifying too many third-party files:
;;(add-hook 'find-file-hook 'whitespace-cleanup)



(defun kill-full-line ()
  "Kills the current line, regardless of the current cursor position. It can be then yanked back with M-y."
  (interactive)
  (let ((orig (point)))
	(beginning-of-line)
	(let ((beg (point)))
	  (forward-line 1)
	  (delete-region beg (point)))
	;; If line is shorter than previous line, then just go to end of line:
	(end-of-line)
	(let ((new (point)))
	  (if (< orig new)
		  (goto-char orig))))
)


(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p 1)
(setq uniquify-ignore-buffers-re "^\\*")


(straight-use-package 'whitespace)
(global-whitespace-mode 1)

;;(setq-default show-trailing-whitespace nil)
;;(setq whitespace-style '(space tabs lines-tail trailing empty indentation space-before-tab space-after-tab))
;; Removed: spaces space-mark tab-mark newline-mark
(setq whitespace-style '(face
	tabs trailing lines space-before-tab newline
	indentation empty space-after-tab))
(setq whitespace-line-column 80)

;; We want to see whether we go past column 80:
;; (currently disabled, as provided by the whitespace mode)
;;(require 'highlight-80+)
;;(add-hook 'find-file-hook 'highlight-80+-mode)


;; 85 width would already allow to display correctly even files with
;; 9999 lines, knowing that the leftmost column for line numbers uses
;; some place. Selecting 88 instead to leave some room to the ... sign
;; used to show a block was folded (anyway the 80-limit is shown by
;; background color).
(add-to-list 'default-frame-alist (cons 'width  88))

;; Depends on the screen height:

;; For a netbook or possibly a laptop:
;;(add-to-list 'default-frame-alist (cons 'height 36))

;; For a normal screen:
;;(add-to-list 'default-frame-alist (cons 'height 56))


(setq my-preferred-font
	  (cond ((eq system-type 'windows-nt) "consolas")
			;;((eq system-type 'gnu/linux) "mono")
			((eq system-type 'gnu/linux) "DejaVu Sans Mono")
			(t nil)))

(setq my-preferred-height
	  (cond ((eq system-type 'windows-nt) 47)
			((eq system-type 'gnu/linux) 56)
			(t nil)))


(when my-preferred-font
  (set-frame-font my-preferred-font nil t))

(when my-preferred-height
  (set-frame-height (selected-frame) my-preferred-height))


;; Key section:


(defun default-f1 ()
  (interactive)
  (message "Default for F1")
)

(defun default-f2 ()
  (interactive)
  (message "Default for F2")
)

(defun default-f3 ()
  (interactive)
  (message "Default for F3")
)

(defun default-f4 ()
  (interactive)
  (message "Default for F4")
)

(defun default-f5 ()
  (interactive)
  (message "Default for F5")
)

(defun default-f6 ()
  (interactive)
  (message "Default for F6")
)

(defun default-f7 ()
 (interactive)
 (message "Default for F7")
)

(defun default-f8 ()
 (interactive)
 (message "Default for F8")
)

(defun default-f9 ()
 (interactive)
 (message "Default for F9")
)

(defun default-shift-f9 ()
 (interactive)
 (message "Default for Shift-F9")
)

(defun default-f10 ()
 (interactive)
 (message "Default for F10")
)

(defun default-f11 ()
 (interactive)
 (message "Default for F11")
)

(defun default-f12 ()
 (interactive)
 (message "Default for F12")
)

(defun save-and-close ()
 (interactive)
 (save-buffer)
 (kill-this-buffer)
)


;; Not triggered when hitting F12, but triggered when hitting F1 on my keyboard:
(global-set-key [f12]               'save-buffer)
(global-set-key [XF86New]           'save-buffer)


(global-set-key "\C-Z" 'undo)

;;(global-set-key "TAB" 'reindent-then-newline-and-indent)



;; Actual mapping:


;; Use M-x describe-key to know to what function a key sequence is bound.

(defun show-assigned-keys ()
  "Shows the current key bindings"
  (interactive)
  (message "F1        -> save-buffer" )
  (message "F2        -> query-replace" )
  (message "F3        -> query-replace-regexp" )
  (message "F4        -> indent-whole-buffer" )
  (message "F5        -> undo" )
  (message "F6        -> repeat-complex-command" )
  (message "F7        -> goto-line" )
  (message "F8        -> whitespace-cleanup" )
  (message "F9        -> fd-switch-dictionary" )
  (message "Shift-F9  -> (currently not bound)" )
  (message "F10       -> save-buffers-kill-emacs" )
  (message "F11       -> (does nothing)" )
  (message "F12       -> (does nothing)" )
)


;; Curiously hitting F1 triggers default-f12:
(global-set-key [f1]			  'default-f1)
(global-set-key [XF86New]		  'default-f1)

;; Usable and behaves like expected:
(global-set-key [f2]              'query-replace)
(global-set-key [XF86Reply]       'query-replace)

;; Usable and behaves like expected:
(global-set-key [f3]			  'query-replace-regexp)
(global-set-key [XF86MailForward] 'query-replace-regexp)

;; Usable and behaves like expected:
(global-set-key [f4]			  'indent-whole-buffer)
(global-set-key [XF86Send]		  'indent-whole-buffer)

;; Curiously bound to Undo:
(global-set-key [f5]              'default-f5)
(global-set-key [XF86New]         'default-f5)

;; Curiously bound to repeat-complex-command:
(global-set-key [f6]			  'default-f6)
(global-set-key [XF86New]		  'default-f6)


;; Usable and behaves like expected:
(global-set-key [f7]			  'goto-line)
(global-set-key [print]			  'goto-line)


;; Usable and behaves like expected:
(global-set-key [f8]              'whitespace-cleanup)
(global-set-key [XF86Save]        'whitespace-cleanup)
(global-set-key [XF86AudioNext]   'whitespace-cleanup)


;; Intercepted by Ubuntu:
(global-set-key [f9]			  'default-f9)
(global-set-key [XF86New]		  'default-f9)


;; Usable and behaves like expected:
(global-set-key [(shift f9)]		'default-shift-f9)
(global-set-key [(shift XF86New)]   'default-shift-f9)
(global-set-key [XF86Explorer]      'default-shift-f9)


;; Usable and behaves like expected:
(global-set-key [f10]				'save-buffers-kill-emacs)
(global-set-key [XF86Documents]     'save-buffers-kill-emacs)


;; Not triggered on my keyboard:
(global-set-key [f11]				'default-f11)
(global-set-key [XF86New]			'default-f11)


;; Not triggered when hitting F12, but triggered when hitting F1 on my keyboard:
(global-set-key [f12]               'save-buffer)
(global-set-key [XF86New]           'save-buffer)


(global-set-key "\C-Z" 'undo)

;;(global-set-key "\C-P" 'recompile)
(global-set-key "\C-P" 'compile)

;; C-s must be dedicated to search, not save, as repeated search will use
;; it anyway:
;;(global-set-key "\C-S" 'save-buffer)
(global-set-key "\C-S" 'isearch-forward)

(global-set-key "\C-Q" 'save-buffer)

(global-set-key "\C-D" 'next-error)
(global-set-key "\C-O" 'find-file)
(global-set-key "\C-F" 'isearch-forward)
(global-set-key "\C-L" 'goto-line)

(global-set-key "\M-k" 'kill-full-line)

(global-set-key [M-right] 'next-buffer)
(global-set-key [M-left]  'previous-buffer)

(global-set-key [delete] 'delete-char)

;;(global-set-key "TAB" 'reindent-then-newline-and-indent)



;; Compilation section.
;; Mostly taken from http://ensiwiki.ensimag.fr/index.php/Dot_Emacs

;; Taken from
;; https://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close:
;; and https://www.emacswiki.org/emacs/ModeCompile

 (defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (when (and
		;; (buffer-live-p buffer)
		 (string-match "compilation" (buffer-name buffer))
		 (string-match "finished" string)
		 (not
		  (with-current-buffer buffer
			(goto-char (point-min))
			(search-forward "warning" nil t))))
	(run-with-timer 1 nil
					(lambda (buf)
					  (bury-buffer buf)
					  (switch-to-prev-buffer (get-buffer-window buf) 'kill))
					buffer)))

(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)


;; ;; make compile window disappear after successful compilation:
;; (setq compilation-finish-function
;;	  (lambda (buf str)
;;		(if (string-match "*Compilation*" (buffer-name buf))
;;			(if (string-match "abnormally" str)
;;				(message "There were errors :-(")
;;			  ;; No errors, make the compilation window go away in 2 seconds:
;;			  (run-at-time 2 nil
;;						   (lambda (buf)
;;							 (delete-windows-on buf)
;;							 (bury-buffer buf))
;;						   buf)
;;			  (message "No errors :-)")))))


;;my-compile is smarter about how to display the new buffer
(defun display-buffer-by-splitting-largest (buffer force-other-window)
  "Display buffer BUFFER by splitting the largest buffer vertically, except if
  there is already a window for it."
  (or (get-buffer-window buffer)
	  (let ((new-win
			 (with-selected-window (get-largest-window)
			   (split-window-vertically))))
		(set-window-buffer new-win buffer)
		new-win)))

(defun my-compile ()
  "Ad-hoc display of compilation buffer."
  (interactive)
  (let ((display-buffer-function 'display-buffer-by-splitting-largest))
	(call-interactively 'compile)))


;; Misc compilation settings:
(setq-default
 compile-command "make"
 compilation-read-command nil
 compilation-scroll-output 'first-error
 compilation-ask-about-save nil
 compilation-window-height 10
 ;; Bad jump location if enabled: compilation-skip-threshold 0
 compilation-auto-jump-to-first-error 1)



;; Spelling section.

;; Hit F9 to toggle English and French dictionaries:


(setq ispell-dictionary "english")
(setq ispell-program-name "aspell")

;; new error: failed to define function flyspell-mode

;;(add-hook 'text-mode-hook 'flyspell-mode)
(dolist (hook '(text-mode-hook))
(add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
 (add-hook hook (lambda () (flyspell-mode -1))))

(defun fd-switch-dictionary()
	  (interactive)
	  (let* ((dic ispell-current-dictionary)
		 (change (if (string= dic "english") "francais" "english")))
		(ispell-change-dictionary change)
		(message "Dictionary switched from %s to %s" dic change)
		))

(global-set-key (kbd "<f9>")                   'fd-switch-dictionary)
(global-set-key (kbd "<XF86AudioLowerVolume>") 'fd-switch-dictionary)


;; Not working apparently:
;;(require 'flyspell-guess)
;;(eval-after-load "flyspell-guess" '(flyspell-insinuate-guess-indicator))


(show-paren-mode t)
(setq show-paren-style 'parenthesis)

(setq initial-scratch-message "")

(setq inhibit-startup-message t)
(setq-default transient-mark-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode 1)

(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)



;; Show line-number in the mode line:
(line-number-mode 1)

;; Show column-number in the mode line:
(column-number-mode 1)

(setq-default fill-column 80)


;; Set cursor color
(set-cursor-color "white")

;; Set mouse color
(set-mouse-color "white")




;; Back-up files, interlock symlinks and autosave files section.
;;
;; These are all different elements, here each stored in a separate
;; ~/.emacs.d/myriad-{backups,interlocks,autosaves} directory.
;;
;; For example, for a '/home/john/tmp/foobar.txt' file, respectively as:
;;    '!home!john!tmp!foobar.txt.~1~' (as a plain file)
;;    '.#!home!john!tmp!foobar.txt' (so as an "hidden symlink")
;;    '#!home!john!tmp!foobar!txt#' (as a plain file)


;; More expensive yet safer settings:
(setq make-backup-files t                   ; backup of a file the first time it is saved.
	  backup-by-copying t               ; don't clobber symlinks
	  version-control t                 ; version numbers for backup files
	  delete-old-versions t             ; delete excess backup files silently
	  delete-by-moving-to-trash t
	  kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
	  kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
	  auto-save-default t               ; auto-save every buffer that visits a file
	  auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
	  auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
	  )


;; Taken from http://snarfed.org/space/gnu%20emacs%20backup%20files:

;; Put autosave files (i.e. #foo#) in one (yet per-user, with no common root, to
;; prevent user-related permission issues) place, *not* scattered all over the
;; filesystem!

;; /tmp is not a good place, as in case of crash/reboot, its content is bound
;; to be lost.
;;
;; (defvar autosave-directory (concat "/tmp/emacs-myriad-autosaves/" (user-login-name) "/"))

;; user-emacs-directory resolves to ~/.emacs.d/:
(defvar autosave-directory (concat user-emacs-directory "myriad-autosaves/"))

(make-directory autosave-directory t)


;;(defun auto-save-file-name-p (filename)
;;  (string-match "^#.*#$" (file-name-nondirectory filename)))

;;(defun make-auto-save-file-name ()
;;  (concat autosave-directory
;;		  (if buffer-file-name
;;			  (concat "#" (file-name-nondirectory buffer-file-name) "#")
;;			(expand-file-name
;;			 (concat "#%" (buffer-name) "#")))))

;;(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Actually works:
(setq auto-save-file-name-transforms `((".*" ,autosave-directory t)))

;; And now for interlock symlinks:
(defvar interlock-directory (concat user-emacs-directory "myriad-interlocks/"))
(make-directory interlock-directory t)
(setq lock-file-name-transforms `((".*" ,interlock-directory t)))

;; Similarly, put backup files (i.e. foo~ or foo.~1~) in one place too.

;; (The backup-directory-alist list contains regexp=>directory mappings;
;; filenames matching a regexp are backed up in the corresponding
;; directory. Emacs will mkdir it if necessary.)

;; /tmp is not a good place, as in case of crash/reboot, content bound to be lost:
;;(defvar backup-directory (concat "/tmp/emacs-myriad-backups/" (user-login-name) "/"))
(defvar backup-directory (concat user-emacs-directory "myriad-backups"))

(make-directory backup-directory t)

;;(setq backup-directory-alist (list (cons "." backup-directory)))
;;(setq backup-directory-alist `(("." . "~/.saves")))
;;(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq backup-directory-alist `((".*" . ,backup-directory)))

(setq vc-make-backup-files t)

(message "<<<<<<######### init.el version 1.3 #########>>>>>>")


(delete-other-windows)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
