;; This is the base, all-purpose Ceylan Emacs configuration.
;;
;; This is an initialisation script written in elisp.
;; Refer to https://www.gnu.org/software/emacs/manual/html_node/elisp/
;;
;; It has been derived from our main init.el - yet only very safe, context-free
;; settings that do not induce dependencies are enabled here.
;;
;; More involved configurations may include it.


;; Not wanting to dismiss the startup screen at each Emacs launch:
(setq inhibit-startup-screen t)

;; Probably useless then:
(setq inhibit-startup-message t)

;; y/n are shorter:
(defalias 'yes-or-no-p 'y-or-n-p)

;; Not wanting the rather large Emacs graphical toolbar to be
;; displayed (at the top):
;;
;; To avoid "Symbol's function definition is void" on console-based
;; Emacs:
(when (display-graphic-p)
   (tool-bar-mode -1))


;; Wanting to follow symbolic links with no specific question:
(setq vc-follow-symlinks t)

;; Revert automatically buffers when the underlying file has changed,
;; if they do not have unsaved changes:
;;
(global-auto-revert-mode 1)

;; Use the M-x recentf-open-files command to be shown a list of recent
;; files:
;;
;; (not deemed useful enough)
;;(recentf-mode 1)


;; Inhibit "Modified buffers exist; exit anyway?" messages:

;; Works properly, bound to "C-x C-c":
(defun save-buffers-kill-emacs ()
  "save some buffers, then exit unconditionally"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))


;; Save previous minibuffer prompts:
(setq history-length 50)
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files:
(save-place-mode 1)

;; Move customization variables to a separate file, to avoid clutter,
;; and load it:
;;
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; So that the configuration files of interest (which should not be directly in
;; the user-emacs-directory ~/.emacs.d apparently) can be found, they are loaded
;; from myriad/conf (e.g. acme-search.el):
;;
(add-to-list 'load-path (file-name-concat (getenv "CEYLAN_MYRIAD") "conf"))


;; Section about autosave and back-up (two different concepts).
;; Taken from http://snarfed.org/space/gnu%20emacs%20backup%20files:

;; Put autosave files (i.e. "#foo#" files) in one (yet per-user, with
;; no common root, to prevent user-related permission issues) place,
;; *not* scattered all over the filesystem!
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

;; Put backup files (i.e. "foo~" files) in one place too.
;;
;; The backup-directory-alist list contains regexp=>directory mappings;
;; filenames matching a regexp are backed up in the corresponding
;; directory, created if necessary.
;;
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))


(add-to-list 'auto-mode-alist '("GNUmake.*\\.inc\\'" . makefile-gmake-mode))

(setq auto-mode-alist
	  (append '(("\\.txt$"  . rst-mode)
				("\\.rst$"  . rst-mode)
				("\\.rst.template$"  . rst-mode)
				("\\.rest$" . rst-mode)) auto-mode-alist))

;; Displays the line numbers also with the next specified extensions:
(add-hook 'find-file-hook 'maybe-activate-line-numbers-mode)

(defun maybe-activate-line-numbers-mode()
  (when (and (stringp buffer-file-name)
             (or (string-match "\\.config\\'" buffer-file-name)
                 (string-match "\\.etf\\'" buffer-file-name)
                 (string-match "\\.xml\\'" buffer-file-name)
				 ))
    (display-line-numbers-mode)))


;; YAML support surprisingly not built-in; so set in
;; init-myriad-erlang-advanced.el



;; We used to prefer splitting the initial window (sized accordingly in the
;; specific init-myriad-local.el at hand), yet this proved actually impractical,
;; as many Emacs are spawned punctually (so, just type C-x 3 when needed):
;;
;;(split-window-horizontally)


;; No more annoying, unmutable bell (e.g. when reaching buffer bounds):
(setq ring-bell-function 'ignore)

;; To be able to move between windows simply thanks to S-<arrow> (i.e. holding
;; shift, and hitting one of the 4 arrow keys; however the block selection
;; becomes then a bit more cumbersome):
;;
(windmove-default-keybindings)

;; Displays the key bindings that can follow any currently entered
;; incomplete command (a prefix), like C-x [...]:
;;
(which-key-mode)

;; Not wanting to accumulate intermediate per-directory buffers:
(setq dired-kill-when-opening-new-dired-buffer 1)


;; No limit in the buffer list:
(setq buffers-menu-max-size nil)

;; Inserting text while the mark is active causes the selected text to
;; be deleted first (and also deactivates the mark):
;;
(delete-selection-mode 1)

;; The default behavior of the mark and region, in which setting the
;; mark activates it and highlights the region:
;;
(transient-mark-mode t)

;; No dialog box popped up, just a prompt in the minibuffer:
;; (setq use-file-dialog nil)

;; More general:
(setq use-dialog-box nil)


;; Highlights the line containing point:
(global-hl-line-mode 1)

;; Display how and whether parentheses (or other delimiters) match up:
(show-paren-mode t)

;; Better than 'parenthesis:
(setq show-paren-style 'mixed)

;; Default message not wanted:
(setq initial-scratch-message "")

(setq frame-title-format '("%b" (buffer-file-name ": %f")))

(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)


;; Show line-number in the mode line:
(line-number-mode 1)

;; Show column-number in the mode line:
(column-number-mode 1)

;; Force some commands to move according to logical lines (i.e. according to the
;; text lines in the buffer, not the visual ones):
;;
(setq line-move-visual nil)

;; Moves the cursor across "physical lines":
;; (finally deactivated, as the 'go to end of line' key was leading to the
;; cursor going downward...)
;;(require 'physical-line)
;;(add-hook 'find-file-hooks 'physical-line-mode-without-exception)


;; Only for older Emacs apparently: '(setq default-tab-width 4)'
(setq-default tab-width 4)
(setq tab-width 4)


;; Automatic indentation while typing:

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

;; Visibly the text-mode-hook is triggered once for prog-mode, and twice for
;; text-mode.
;;
(defun fix-behaviours-for-text-modes ()

  (message "############## Fixing behaviours for text modes ###########")

  ;; Advanced automatic indentation not adapted to text modes:
  (remove-hook 'find-file-hooks 'set-advanced-ret-behaviour)

  ;; This basic indentation is fine with text modes:
  (global-set-key (kbd "RET") 'newline-and-indent)

  ;; Long lines are normal in text modes:
  ;;(remove-hook 'find-file-hook 'highlight-80+-mode)

  ;; Surely an hack, but works great:
  ;;(setq-local whitespace-line-column 9999)

  ;; No 'lines' or 'empty':
  (setq-local whitespace-style '(face
	tabs trailing space-before-tab newline
	indentation space-after-tab))

  ;; Not wanting auto-completion (e.g. company) to apply when entering texts:
  (setq lsp-completion-provider :none)

  ;; Not available at this point:
  ;;(company-mode -1)
  ;;(company-box-mode -1)

  )

(add-hook 'text-mode-hook 'fix-behaviours-for-text-modes)


(setq scroll-step 1)

(setq-default fill-column 80)

;; Set cursor color:
(set-cursor-color "white")

;; Color for the cursor line:
;;(set-face-background 'highlight "black")
(set-face-background 'highlight "gray19")

;; Turns off any blinking cursor:
(if (fboundp 'blink-cursor-mode)
	(blink-cursor-mode -1))


;; Set mouse color:
(set-mouse-color "white")

;; Set foreground and background colors:
(set-foreground-color "white")

;;(set-background-color "darkblue")
(set-background-color "black")


;; Set highlighting colors for isearch and drag:
;;(set-face-foreground 'highlight "white")

(set-face-foreground 'region "black")
(set-face-background 'region "lightgreen")

;;(set-face-foreground 'secondary-selection "skyblue")
;;(set-face-background 'secondary-selection "darkblue")

(set-face-foreground 'secondary-selection "red")
(set-face-background 'secondary-selection "green")

;; Low brightness of the background preferred:
;;(setq frame-background-mode 'dark)
'(frame-background-mode (quote dark))


;; Hook section:

;; Displays the line numbers on the left of the editor, in all
;; programming modes:
;;
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Also useful, for the debugging of document generation:
(add-hook 'rst-mode-hook 'display-line-numbers-mode)


;; Function section:

;; whitespace-mode is built-in:
(global-whitespace-mode 1)

;;(setq-default show-trailing-whitespace nil)

;; Use C-h v whitespace-style for documentation.
;;
;; Removed: spaces space-mark tab-mark newline-mark indentation (e.g. not
;; wanting a yellow rectangle from beginning of line to first non-whitespace
;; character)
;;
(setq whitespace-style '(face
	tabs trailing lines space-before-tab newline
	empty space-after-tab))

(setq whitespace-line-column 80)

;; We want to see whether we go past column 80:
;; (currently disabled, as provided by the whitespace mode)
;;(require 'highlight-80+)
;;(add-hook 'find-file-hook 'highlight-80+-mode)


;; Indenting buffers as a whole:
;; (more info: https://www.emacswiki.org/emacs/DeletingWhitespace#h5o-11)
(defun indent-whole-buffer ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  )

;;(add-hook 'find-file-hook 'indent-whole-buffer)


;; whitespace-cleanup is built-in:
;;(add-hook 'find-file-hook 'whitespace-cleanup)


;; To display the names of buffers corresponding to identical filenames (but
;; different paths):
;;
;; Preferring default: (setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p 1)
(setq uniquify-ignore-buffers-re "^\\*")


(defun kill-full-line ()
  "Kills the current line as a whole, regardless of the current cursor position. It can be then yanked back with M-y."
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



;; Compilation section.


;; Taken from
;; https://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close:
;;
(defun bury-compile-buffer-if-successful (buffer string)
 "Bury the compilation buffer (BUFFER STRING) if succeeded without warnings."
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



;; Previous implementation:

;; Mostly taken from http://ensiwiki.ensimag.fr/index.php/Dot_Emacs

;; Makes the compile window disappear after a successful compilation:
;;(setq compilation-finish-function
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


;;(defun display-buffer-by-splitting-largest (buffer force-other-window)
;;  "Display buffer BUFFER by splitting the largest buffer vertically, except
;;if there is already a window for it."
;;  (or (get-buffer-window buffer)
;;	  (let ((new-win
;;			 (with-selected-window (get-largest-window)
;;			   (split-window-vertically))))
;;		(set-window-buffer new-win buffer)
;;		new-win)))


;; Smarter about how to display the new buffer:
;;(defun myriad-compile ()
;;  "Ad-hoc display of compilation buffer."
;;  (interactive)
;;  (let ((display-buffer-function 'display-buffer-by-splitting-largest))
;;	(call-interactively 'compile)))



;; Misc compilation settings:
(setq-default
 compile-command "make"
 compilation-read-command nil
 compilation-scroll-output 'first-error
 compilation-ask-about-save nil
 compilation-window-height 10
 compilation-skip-threshold 0
 compilation-auto-jump-to-first-error 1)


(defun save-and-close ()
 (interactive)
 (save-buffer)
 (kill-this-buffer)
)



(defun show-assigned-keys ()
  "Shows the current key bindings (not updated yet). Anyway the use of F*
keys can be seen as counterproductive."
  (interactive)
  (message "F1        -> save-buffer" )
  (message "F2        -> query-replace" )
  (message "F3        -> query-replace-regexp" )
  (message "F4        -> indent-whole-buffer" )
  (message "F5        -> undo" )
  (message "F6        -> repeat-complex-command" )
  (message "F7        -> goto-line" )
  (message "F8        -> whitespace-cleanup" )
  (message "F9        -> switch-dictionary" )
  (message "Shift-F9  -> (currently not bound)" )
  (message "F10       -> save-buffers-kill-emacs" )
  (message "F11       -> (does nothing)" )
  (message "F12       -> (does nothing)" )
)

;; These defaults allow to test keys:

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


;; Actual key mapping section.
;;
;; Note that they may be redefined by including configurations (which
;; might offer extra features based on extra dependencies).
;;
;; Use M-x describe-key to know to what function a key sequence is bound.

(global-set-key [delete] 'delete-char)

;;(global-set-key "TAB" 'reindent-then-newline-and-indent)

;; Not necessary (already built-in):
;; - already offered by C-s:
;;  * (global-set-key "\C-S" 'isearch-forward)
;;  * (global-set-key "\C-F" 'isearch-forward)
;; - C-x s: save current buffer

;; As C-r already taken:
(global-set-key "\C-D" 'replace-string)

;; Maybe better than F8:
(global-set-key "\C-O" 'whitespace-cleanup)

(global-set-key "\C-P" 'recompile)

(global-set-key "\C-Q" 'next-error)
(global-set-key (kbd "s-q") 'quoted-insert)

(global-set-key "\C-Z" 'undo)
(global-set-key "\C-L" 'goto-line)

(global-set-key (kbd "<M-up>") 'scroll-down-command)
(global-set-key (kbd "<M-down>") 'scroll-up-command)

(global-set-key (kbd "s-c") 'comment-dwim) ;; c for comment/uncomment

(setq smerge-command-prefix "\C-cv")


;; Obsolete:
;;(standard-display-european 1)


;; See windmove:
(global-set-key [M-right] 'next-buffer)
(global-set-key [M-left]  'previous-buffer)

;; Instead of "kill forward to the end of the sentence" (kill-sentence):
(global-set-key "\M-k" 'kill-full-line)





;; Finally, management of all F* keys:
;;
;; (many are disabled, to encourage the use of regular key combinations)


;; Curiously hitting F1 triggers default-f12:
(global-set-key [f1]			  'default-f1)
(global-set-key [XF86New]		  'default-f1)


;; Usable and behaves like expected:
;;(global-set-key [f2]            'query-replace)
(global-set-key [f2]              'default-f2)

;;(global-set-key [XF86Reply]     'query-replace)
(global-set-key [XF86Reply]       'default-f2)


;; Usable and behaves like expected:
;;(global-set-key [f3]			  'query-replace-regexp)
(global-set-key [f3]			  'default-f3)

;;(global-set-key [XF86MailForward] 'query-replace-regexp)
(global-set-key [XF86MailForward]   'default-f3)


;; Usable and behaves like expected:
;;(global-set-key [f4]			  'indent-whole-buffer)
(global-set-key [f4]			  'default-f4)

;;(global-set-key [XF86Send]	  'indent-whole-buffer)
(global-set-key [XF86Send]	  'default-f4)


;; Curiously bound to Undo:
(global-set-key [f5]              'default-f5)
(global-set-key [XF86New]         'default-f5)


;; Curiously bound to repeat-complex-command:
(global-set-key [f6]			  'default-f6)
(global-set-key [XF86New]		  'default-f6)


;; Usable and behaves like expected:
;;(global-set-key [f7]			  'goto-line)
(global-set-key [f7]			  'default-f7)

;;(global-set-key [print]		  'goto-line)
(global-set-key [print]			  'default-f7)


;; Usable and behaves like expected:
;;(global-set-key [f8]            'whitespace-cleanup)
(global-set-key [f8]              'default-f8)

;;(global-set-key [XF86Save]      'whitespace-cleanup)
(global-set-key [XF86Save]        'default-f8)

;;(global-set-key [XF86AudioNext] 'whitespace-cleanup)
(global-set-key [XF86AudioNext]   'default-f8)


;; Intercepted by Ubuntu:
(global-set-key [f9]						   'switch-dictionary)
;;(global-set-key [f9]						   'default-f9)

;;(global-set-key (kbd "<XF86AudioLowerVolume>") 'switch-dictionary)
(global-set-key (kbd "<XF86AudioLowerVolume>") 'default-f9)

;;(global-set-key [XF86New]					   'switch-dictionary)
(global-set-key [XF86New]					   'default-f9)


;; Usable and behaves like expected:
;;(global-set-key [(shift f9)]		  'default-shift-f9)
;;(global-set-key [(shift XF86New)]   'default-shift-f9)
;;(global-set-key [XF86Explorer]      'default-shift-f9)


;; Usable and behaves like expected:
;;(global-set-key [f10]				'save-buffers-kill-emacs)
(global-set-key [f10]				'default-f10)

;;(global-set-key [XF86Documents]   'save-buffers-kill-emacs)
(global-set-key [XF86Documents]     'default-f10)

;; Standard:
(global-set-key (kbd "C-x C-c")     'save-buffers-kill-emacs)


;; Not triggered on my keyboard:
(global-set-key [f11]				'default-f11)
(global-set-key [XF86New]			'default-f11)

(global-set-key (kbd "s-l") 'switch-dictionary) ;; l for language



;; Not triggered when hitting F12, but triggered when hitting F1 on my keyboard:
;;(global-set-key [f12]               'save-buffer)
;;(global-set-key [XF86New]           'save-buffer)


(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)


;; Font section.



;; Our default font name (no size wanted here, as to be set only in local
;; settings):
;;
;; Usually, at least when requiring "Monospace-11", actual default is:
;; "-UKWN-Nimbus Mono PS-regular-normal-normal".
(setq myriad-font-name "Monospace")


;; Could be added (when initial-window-system ;;
(defun set-myriad-font-p (font-name)
  (if (member font-name (font-family-list))
	  (setq myriad-font-name font-name)))


;; By increasing priority of fonts:
(set-myriad-font-p "DejaVu Sans Mono")
(set-myriad-font-p "Iosevka Fixed")
(set-myriad-font-p "JetBrainsMono Nerd Font")

(message "Myriad-selected font name: %s" myriad-font-name)



;; Does not seem to apply (two buffers still shown in two window panes
;; if two files specified on the command-line):
;;
;;(delete-other-windows)



;; Spelling section.

;; Evaluate switch-dictionary or hit s-l (previously:F9) to toggle the English
;; (default) and French dictionaries:

(setq ispell-dictionary "english")
(setq ispell-program-name "aspell")

(defun switch-dictionary()
	(interactive)
	(let* ((dic ispell-current-dictionary)
		(change (if (string= dic "english") "francais" "english")))
		(ispell-change-dictionary change)
	;; Otherwise buffer is not rescanned:
	(flyspell-buffer)

	;; Not wanting auto-completion (e.g. company) to apply when entering texts:
    ;;(setq lsp-completion-provider :none)

	(message "Dictionary switched from %s to %s" dic change)
	))


;; new error: failed to define function flyspell-mode

;;(add-hook 'text-mode-hook 'flyspell-mode)
(dolist (hook '(text-mode-hook))
 (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
 (add-hook hook (lambda () (flyspell-mode -1))))


;; Not working apparently:
;;(require 'flyspell-guess)
;;(eval-after-load "flyspell-guess" '(flyspell-insinuate-guess-indicator))



;; Mouse search section.

;; Do not consider underscores and dashes as word separators (otherwise
;; mouse-based search changes its selection during search):
;;
;; (probably a bad idea, search patterns seem not to be found when having a
;; prefix)
;;
(global-superword-mode 1)



;; For proper mouse search:

;; Specific functions would have to be defined to act on a selection:
;;(global-set-key [(mouse-3)] 'isearch-forward)
;;(global-set-key [(shift mouse-3)] 'isearch-backward)

;; The only functional, bearable mouse-based search that could be found is
;; acme-search.

;; It is not available as a package:
;;(use-package acme-search :ensure (:wait t) :demand t)
;;(add-to-list 'load-path "~/.emacs.d/")

;; So we distribute it directly by ourselves our version of it, therefore this
;; does not involve a specific package; moreover it is already available in the
;; load-path. It can therefore belong to this base:

(require 'acme-search)

(global-set-key [(mouse-3)] 'acme-search-forward)
(global-set-key [(shift mouse-3)] 'acme-search-backward)

;; Possibly better:
(setq mouse-drag-copy-region t)


;; To centralise Emacs instances, rather than having one per need:
;;(server-start)

(require 'server)

(or (server-running-p)
	(server-start))

;; Also tried:
;;(load "server")
;;(unless (server-running-p) (server-start))


;; No more question about clients being still there:
;; (must be *after* server-start)
;; (yet does not seem to be taken into account)
;;
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)


;; The Auto Fill mode is finally not so useful, more of a nuisance for text
;; modes or even programming modes:
;;
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;;(add-hook 'prog-mode-hook 'turn-on-auto-fill)
;;(add-hook 'erlang-mode-hook 'turn-on-auto-fill)



;; Does not seem to apply (two buffers still shown in two window panes
;; if two files specified on the command-line):
;;
;;(delete-other-windows)


;; So that it can be loaded with 'require':
(provide 'init-myriad-base)
