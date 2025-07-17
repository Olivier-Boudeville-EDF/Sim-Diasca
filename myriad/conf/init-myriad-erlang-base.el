;; This is a minimal configuration of Emacs regarding Erlang.
;;
;; Only the base Erlang-related settings are specified here, i.e. the ones
;; covered by the very long https://erlang.org/download/contrib/erlang.el file
;; (hence, here, no LSP and all).
;;

;; This configuration can be tested just by itself that way (here we chose to
;; create a dedicated directory within the Emacs configuration one):
;;
;; $ DIR=~/.emacs/test-init-erlang-base && mkdir $DIR && cd $DIR
;; $ ln -s $CEYLAN_MYRIAD/conf/init-erlang-base.el init.el
;; $ emacs --init-directory $DIR foo.erl

;; See also https://www.erlang.org/doc/apps/tools/erlang-el.html

;; Adapted from the README distributed with the OTP tarballs.



;; For us, there are two possible conventional locations for an Erlang install:
;;  - either in the user account (in ~/Software/Erlang/Erlang-current-install)
;;  - or directly in the system tree (in /usr/local/)


(message "Entering init-myriad-erlang-base.el")

;; Auto-detect location, priority being the user account:
(setq user-erlang-base-path '"~/Software/Erlang/Erlang-current-install")
(setq system-erlang-base-path '"/usr/local")

;; Note that 'emacs' is here a symbolic link typically created by our
;; install-erlang.sh script, so that upgrading Erlang does not risk making the
;; actual directory (e.g. lib/tools-2.8.2/emacs) vanish because of a change in
;; the 'tools' version (thus requiring the current configuration file to be
;; endlessly modified):
;;
(if (file-directory-p user-erlang-base-path)
	(progn (message "Using user-level Erlang install.")
		   (setq erlang-root-dir user-erlang-base-path)
		   ;;(setq erlang-emacs-dir (concat erlang-root-dir "/emacs")))
		   (setq erlang-emacs-dir (concat erlang-root-dir "/lib/erlang/emacs")))
    (if (file-directory-p system-erlang-base-path)
		(progn (message "Using system-level Erlang install.")
			   (setq erlang-root-dir system-erlang-base-path)
			   (setq erlang-emacs-dir (concat erlang-root-dir "/lib/erlang/emacs")))
		(error "No Erlang installation found.")))


(message (concat "Emacs for Erlang located at " erlang-emacs-dir))

(setq load-path (cons erlang-emacs-dir load-path))


(setq erlang-bin-dir (concat erlang-root-dir "/lib/erlang/bin"))


(setq exec-path (cons erlang-bin-dir exec-path))


;; If ever needed (apparently not):
;;(setq auto-mode-alist
;;	  (append '(("\\.escript$" . erlang-mode)) auto-mode-alist))


;; Recommended by Erlang's CONTRIBUTING.md:

(add-hook 'erlang-mode-hook 'my-erlang-hook)

(defun my-erlang-hook ()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4))


;; Not really useful (see
;; https://doc.endlessparentheses.com/Fun/auto-raise-mode.html):
;; (auto-raise-mode t)


;; Allows to have Emacs automatically insert newlines to word-wrap:
;; (see https://www.emacswiki.org/emacs/AutoFillMode)
;;(defun my-erlang-mode-hook () (turn-on-auto-fill) )

(require 'erlang-start)



;; Not wanting single '%' to be set at the default column 48:
(add-hook 'erlang-mode-hook (lambda () (setq-local comment-column 0)))

;; erlang-electric-semicolon removed, as more a nuisance than a help (function
;; headers generally pasted from first):
;;
(setq erlang-electric-commands '(erlang-electric-comma
                                 erlang-electric-gt))


;; So that it can be loaded with 'require':
(provide 'init-myriad-erlang-base)

(message "Leaving init-myriad-erlang-base.el")
