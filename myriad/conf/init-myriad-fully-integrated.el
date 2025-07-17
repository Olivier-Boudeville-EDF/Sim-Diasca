;; This is the fully-integrated Ceylan Emacs configuration, for daily usage.


;; To find the next init-myriad-base.el:
(add-to-list 'load-path (file-name-concat (getenv "CEYLAN_MYRIAD") "conf"))


;; For all general-purpose basics:
;;
;; (loads it iff it has not been loaded already)
;;
;; Apparently 'require' triggers package.el, leading to the following warning
;; in some versions: "Warning (emacs): Package.el loaded before Elpaca".
;;
(require 'init-myriad-base)


;; For the Erlang base configuration:
(require 'init-myriad-erlang-base)


;; For the C/C++ base configuration:
(require 'init-myriad-c-cpp-base)


;; For the Python base configuration:
(require 'init-myriad-python-base)


;; For host-specific settings; intentionally searched in local configuration
;; directory, so that none applies by default (the one in myriad/conf is just an
;; example).
;;
;; Not done later, as the local settings may specify the use of a proxy, which
;; must be setup priori to the package management below:
;;
(setq local-conf-file "~/.emacs.d/init-myriad-local.el")

(if (file-exists-p local-conf-file) (require 'init-myriad-local) nil)


;; To be able to rely on a package manager:
(require 'init-myriad-package-management)


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
