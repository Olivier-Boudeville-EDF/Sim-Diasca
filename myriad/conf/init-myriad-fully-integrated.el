;; This is the fully-integrated Ceylan Emacs configuration, for daily usage.;


;; Note that our early-init.el file shall be available (as a regular file or a
;; symlink) in one's user-emacs-directory, typically in ~/.emacs.d/ in order to
;; avoid the infamous 'Package.el loaded before Elpaca' warning.



;; To find the next init-myriad-*.el files, we put the Myriad configuration
;; directory in **last** position, so that it is used as a last resort (instead
;; of shadowing any file that the user would have put before).

;; Note that the ~/.emacs.d directory is by default not in the load-path, as "it
;; would be likely to cause problems", so we recommend using any
;; ~/.emacs.d/myriad-local-override directory instead; to let the user define
;; overriding files there, we add it as well, just prior to the aforementioned
;; Myriad configuration directory (both at end of original load-path):

(push "~/.emacs.d/myriad-local-override" (cdr (last load-path)))
(push (file-name-concat (getenv "CEYLAN_MYRIAD") "conf") (cdr (last load-path)))


;; If instead preferring adding these Myriad-related configuration directories
;; at the **beginning** of the load-path, for an increased robustness:
;;
;; (add-to-list 'load-path (file-name-concat (getenv "CEYLAN_MYRIAD") "conf"))
;; (add-to-list 'load-path "~/.emacs.d/myriad-local-override")


;;(message "The load path is: %s" load-path)
;;(print load-path)


;; For all general-purpose basics:
;;
;; (loads it iff it has not been loaded already)
;;
(require 'init-myriad-base)


;; For the Erlang base configuration:
;;(load-file "init-myriad-erlang-base.el")
(require 'init-myriad-erlang-base)


;; To be able to rely on a package manager:
(require 'init-myriad-package-management)


;; For the C/C++ base configuration:
;;(load-file "init-myriad-c-cpp-base.el")
(require 'init-myriad-c-cpp-base)

;; For the Python base configuration:
;;(load-file "init-myriad-python-base.el")
(require 'init-myriad-python-base)

;;(message "Before myriad-local")

;; Not done later, as the local settings may specify the use of a proxy, which
;; must be setup prior to the package management below:

;; Hence to be found through the original load path, otherwise in ~/.emacs.d,
;; otherwise in Myriad configuration directory:

(setq myriad-conf-file "init-myriad-local.el")

(if (locate-file myriad-conf-file load-path)
    ;;(load-file "init-myriad-local.el")
    (require 'init-myriad-local)
    (message
	 (concat "(no '" myriad-conf-file "' Myriad local configuration found)")))

;;(message "After myriad-local")


;; From this point 'require' can be preferred to 'load-file'.

;; For the RST base configuration:
(require 'init-myriad-rst-base)


;; Replaces flymake; for Erlang, triggers rebar3 if such a project is found:
;; (refer to https://www.flycheck.org/en/latest/languages.html#erlang)
;;
;;(use-package flycheck
;;  :ensure t (:wait t) :demand t
;;  :init (global-flycheck-mode))


;; For more advanced Erlang configuration, requiring packages:
(require 'init-myriad-erlang-advanced)



;; So that it can be loaded with 'require':
(provide 'init-myriad-fully-integrated)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
