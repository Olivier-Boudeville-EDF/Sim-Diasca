;; This is our configuration of Emacs regarding package management.
;;
;; After relying on package.el then straight.el, we use now Elpaca.


;; Disabling package.el in our "early"-init file (i.e. "early-init.el") works,
;; whereas '(setq package-enable-at-startup nil)' not.


;; Taken verbatim from
;; https://github.com/progfolio/elpaca?tab=readme-ov-file#installer.
;;
;; It will clone/build/activate Elpaca into our user-emacs-directory, under the
;; 'elpaca' subdirectory:
;;
;; If the operation fails with "Cannot open load file, No such file or
;; directory, elpaca", it may be solved by removing the ~/.emacs.d/elpaca
;; directory (possibly created yet no valid to a proxy problem...)


(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


;; End of verbatim section.



;; Install use-package support:
(elpaca elpaca-use-package
   ;; Enable use-package :ensure support for Elpaca.
   (elpaca-use-package-mode))


;; Then packages (e.g. a foo one) are to be requested (here, synchronously,
;; thanks to :wait, via:
;;
;;(use-package foo :ensure (:wait t) :demand t)


;; We finally prefer our display settings:
;;
;;(use-package catppuccin-theme :ensure (:wait t) :demand t)
;;(load-theme 'catppuccin :no-confirm)


;; So that it can be loaded with 'require':
(provide 'init-myriad-package-management)
