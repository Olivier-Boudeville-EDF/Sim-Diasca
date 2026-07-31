;; This early-init.el shall be put (as a regular file or a symlink) in one's
;; user-emacs-directory, typically in ~/.emacs.d/.
;;
;; Its sole purpose is to avoid the infamous 'Package.el loaded before Elpaca'
;; warning; no other solution worked.
;;
;; See
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html

(setq package-enable-at-startup nil)
