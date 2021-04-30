;; This is an initialization script written in elisp.
;; Refer to https://www.gnu.org/software/emacs/manual/html_node/elisp/


;; Use our 'update-emacs-modules.sh' script to update the *.el files of
;; interest.

(setq load-path (cons "~/.emacs.d/my-modules" load-path))

;; No more annoying, unmutable bell (ex: when reaching buffer bounds):
(setq ring-bell-function 'ignore)


;; To avoid having the Warnings buffer be opened with a message like:
;;
;; """
;; Warning (server): Unable to start the Emacs server.
;; There is an existing Emacs server, named "server".
;; To start the server in this Emacs process, stop the existing
;; server or call ‘M-x server-force-delete’ to forcibly disconnect it.
;; """
;;
;; when opening an extraneous emacs:
;;
;; (does not seem to work, though, hence commented-out)
(require 'server)
(or (server-running-p)
	(server-start))
;;
;; Also tried:
;;(load "server")
;;(unless (server-running-p) (server-start))

;; Compiles .el files newer than their .elc counterpart, or not having one:
;; One can also use M-x byte-compile-file to precompile .el files (ex: linum).
;; Warning: apparently, unless the .elc file is removed, changes will be
;; effective only when having started emacs again *twice*.
;; Now disabled, to avoid spurious errors, like load-path not being then found,
;; or complaints about free variables:
;;(byte-recompile-directory "~/.emacs.d" 0)


;; RST files support section.

;; May be disabled if slowing emacs down way too much:
(require 'rst)
(setq auto-mode-alist
	  (append '(("\\.txt$"  . rst-mode)
				("\\.rst$"  . rst-mode)
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



;; Erlang support:

;; Adapted from the README distributed with the OTP tarballs:

;; Note: 'emacs' is here a symbolic link typically created by our
;; install-erlang.sh script, so that upgrading Erlang does not risk
;; to make this directory (ex: lib/tools-2.8.2/emacs) vanish because
;; of a change in the 'tools' version (thus requiring the current file
;; to be endlessly modified)

;; Two possible conventional locations for an Erlang install:
;;  - either in the user account (in ~/Software/Erlang/Erlang-current-install)
;;  - or directly in the system tree (in /usr/local/lib/erlang/)

(setq load-path (cons "~/Software/Erlang/Erlang-current-install/lib/erlang/emacs" load-path))
(setq load-path (cons "/usr/local/lib/erlang/emacs" load-path))

;;(setq erlang-root-dir "~/Software/Erlang/Erlang-current-install/lib/erlang")
;;(setq erlang-root-dir "/usr/local/lib/erlang")

(setq exec-path (cons "~/Software/Erlang/Erlang-current-install/lib/erlang/bin" exec-path))
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))

;; erlang-electric-semicolon removed, as more a nuisance than a help (function
;; headers generally pasted from first):
;;
(setq erlang-electric-commands '(erlang-electric-comma
								 erlang-electric-g))


;; Only in some language modes, not all text modes nor even all programming
;; modes where it is more of a nuisance:
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;;(add-hook 'prog-mode-hook 'turn-on-auto-fill)
;; Not so useful even here:
;;(add-hook 'erlang-mode-hook 'turn-on-auto-fill)

(require 'erlang-start)



;; Not used anymore, as faulty errors are triggered when using WOOPER:
;;(require 'erlang-flymake)

;; To be able to move between windows simply thanks to S-<arrow> (i.e. holding
;; shift, and hitting one of the 4 arrow keys; however the block selection
;; becomes then a bit more cumbersome):
;;
(windmove-default-keybindings)

;; No limit:
(setq buffers-menu-max-size nil)


;; Taken from
;; https://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close:

(defun bury-compile-buffer-if-successful (buffer string)
 "Bury a compilation buffer if succeeded without warnings "
 (when (and
		 (buffer-live-p buffer)
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


(setq auto-mode-alist
	  (append '(("\\.escript$"  . erlang-mode)) auto-mode-alist))

;;(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

;; Allows to have Emacs automatically insert newlines to word-wrap:
;; (see https://www.emacswiki.org/emacs/AutoFillMode)
;;(defun my-erlang-mode-hook () (turn-on-auto-fill) )

(message "<<<<<<######### init.el version 1.0 #########>>>>>>")

;; Indentation:
;; Starting from its second line, a multi-line statement should be
;; indented of 2 characters from the beginning of line, not relatively
;; to, say, the opening parenthesis which can be close to the right edge
;; of the line.
(setq c-offsets-alist '(
		;; Otherwise parameters are aligned with the first, whereas we want a
		;; fixed offset:
		(arglist-cont-nonempty . 2)
		(arglist-intro . 2)
						)
)


;; Support for C-like languages:
;; (customizations for all of c-mode, c++-mode, objc-mode, java-mode)
(defun my-c-mode-common-hook ()
  (setq cc-default-style "bsd")
  (c-set-offset 'substatement-open 0)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'cc-mode-common-hook 'my-c-mode-common-hook)


;; Displaying of line number on the left:
;; (see also 'longlines')
(require 'linum)
(add-hook 'find-file-hook (lambda () (linum-mode 1)))


;; Not working apparently with emacs 22.2.1:
;;(auto-raise-mode t)


;; Moves the cursor across "physical lines":
;; (finally deactivated, as the 'go to end of line' key was leading to the
;; cursor going downward...)
;;(require 'physical-line)
;;(add-hook 'find-file-hooks 'physical-line-mode-without-exception)



;; Automatic indentation while typing:

;; Does not work correctly with inner bullet lists:
;;(setq indent-line-function 'indent-relative-maybe)

;; Just indents by default at the same level when Enter is hit:
;;(add-hook 'find-file-hooks '(lambda ()
;;      (local-set-key (kbd "RET") 'newline-and-indent)))


;; Useful for most programming modes, but disrupts sub-bullet lists in
;; text (ex: RST) modes (puts them all at the same level):
;; (not defined as a lambda in order to be able to remove it)
(defun set-advanced-ret-behaviour ()
  ;;(message "############ Setting advanced RET behaviour ###########")
  ;;(local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
  (global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
  )


;;(add-hook 'find-file-hooks 'set-advanced-ret-behaviour)



(defun fix-behaviours-for-text-modes ()
  (message "############## Fixing behaviours for text modes ###########")

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



;; Indenting buffers as a whole:
(defun indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  )

;;(add-hook 'find-file-hook 'indent-whole-buffer)
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


(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p 1)
(setq uniquify-ignore-buffers-re "^\\*")

(require 'whitespace)
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
(add-to-list 'default-frame-alist (cons 'height 56))


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
(global-set-key "\C-P" 'recompile)

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

;; make compile window disappear after successful compilation:
(setq compilation-finish-function
	  (lambda (buf str)
		(if (string-match "*Compilation*" (buffer-name buf))
			(if (string-match "abnormally" str)
				(message "There were errors :-(")
			  ;; No errors, make the compilation window go away in 2 seconds:
			  (run-at-time 2 nil
						   (lambda (buf)
							 (delete-windows-on buf)
							 (bury-buffer buf))
						   buf)
			  (message "No errors :-)")))))

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
 compilation-skip-threshold 0
 compilation-auto-jump-to-first-error 1)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)
(transient-mark-mode t)


(setq use-file-dialog nil)

;;(standard-display-european 1)


;; Spelling section.

;; Hit F9 to toggle english and french dictionaries:


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



;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

(setq-default fill-column 80)

;; Set cursor color
(set-cursor-color "white")

;; Set mouse color
(set-mouse-color "white")

;; Do not consider underscores and dashes as word separators (otherwise
;; mouse-based search changes its selection during search):
;;
;; (probably a bad idea, search patterns seem not to be found when having a
;; prefix)
;;
;;(global-superword-mode 1)


;; For proper mouse search:
(require 'acme-search)
(global-set-key [(mouse-3)] 'acme-search-forward)
(global-set-key [(shift mouse-3)] 'acme-search-backward)



;; Set foreground and background
(set-foreground-color "white")
;;(set-background-color "darkblue")
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

(setq vc-follow-symlinks nil)
(setq line-move-visual nil)

;; Only for older Emacs apparently:(setq default-tab-width 4)
(setq-default tab-width 4)

(setq tab-width 4)

(setq scroll-step 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(transient-mark-mode 1)
(savehist-mode 1)

(setq frame-title-format '("%b" (buffer-file-name ": %f")))


;; Wrong: (setq default-tab-width 4)
(setq-default tab-width 4)

(setq tab-width 4)


(setq tool-bar-mode nil)

(server-start)

;; No more question about clients being still there:
;; (must be *after* server-start)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)



;; Back-up and autosave section.
;; Taken from http://snarfed.org/space/gnu%20emacs%20backup%20files:

;; Put autosave files (ie #foo#) in one (yet per-user, with no common root, to
;; prevent user-related permission issues) place, *not* scattered all over the
;; filesystem!
;;
(defvar autosave-dir
  (concat "/tmp/emacs-myriad-autosaves-" (user-login-name) "/"))

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
		  (if buffer-file-name
			  (concat "#" (file-name-nondirectory buffer-file-name) "#")
			(expand-file-name
			 (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))




(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))


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

(delete-other-windows)
