;; This is an optional, host-local Emacs configuration.
;; It provides an example to customise host-specific elements.
;;
;; To avoid committing its local, host-specific changes, we recommend executing:
;; 'git update-index --skip-worktree init-myriad-local.el'.


;;(message "Applying local settings")



;; A problem with most font specifications is that they specify both the font
;; name (albeit best selected in the generic configuration, based on theactual
;; font availability) and its size (which is a local setting, hence best placed
;; here).


;; Often at least a bit relative to a font name (here: "JetBrainsMono Nerd
;; Font"):


;; For a normal-resolution screen:
;;(setq myriad-font-size "8")


;; For some base laptop resolution screen or high-resolution screens:
;; (9 resolved in 12, 10 in 13, 11 in 15)
(setq myriad-font-size "10")



;; Based on myriad-font-name, the best font found available by our base
;; configuration:
;;
(setq myriad-full-font (concat myriad-font-name "-" myriad-font-size))

(message "Myriad-selected full font: %s" myriad-full-font)

(set-frame-font myriad-full-font t t)




;; Defines the default size of the Emacs window ("frame", in Emacs-speak).
;; (units are characters)

;; 85 width would already allow to display correctly even files with
;; 9999 lines, knowing that the leftmost column for line numbers uses
;; some place. Selecting 88 instead to leave some room to the ... sign
;; used to show a block was folded (anyway the 80-limit is shown by
;; background color).
;;
(add-to-list 'default-frame-alist (cons 'width  90))


;; For two buffers side by side, on a normal screen:

;; Just above 85 characters:
;;(add-to-list 'default-frame-alist (cons 'width  180))

;; Almost full screen width for a laptop:
;;(add-to-list 'default-frame-alist (cons 'width  204))



;; Depends on the screen height:

;; For some netbook:
;;(add-to-list 'default-frame-alist (cons 'height 36))

;; For some laptop:
;; (at least relevant for our "10" font size:
(add-to-list 'default-frame-alist (cons 'height 53))

;; For a normal screen:
;;(add-to-list 'default-frame-alist (cons 'height 56))

;; For a larger screen:
;;(add-to-list 'default-frame-alist (cons 'height 124))



;; Setting the proxy information on the command line prior to executing Emacs is
;; another, possibly more reliable, option:
;;
;;(setq url-proxy-services
;;    '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;      ("https"    . "foobar.org:1030")
;;      ("http"     . "foobar.org:1030")))


;; So that it can be loaded with 'require':
(provide 'init-myriad-local)
