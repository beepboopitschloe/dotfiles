;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up use-package before anything else

(load-file "~/.emacs.d/rose/package.el")
(load-file "~/.emacs.d/rose/lsp.el")

;;;;;;;;;;;;;;;;;
;; basic packages

(use-package evil :ensure t)
(use-package evil-collection :ensure t
             :config
             (setq evil-want-keybinding nil)
             (evil-collection-init))
(use-package magit :ensure t)
(use-package company :ensure t :straight t)
(use-package which-key :ensure t :straight t)


;;;;;;;
;; helm

(use-package helm
  :straight t)

;;;;;;;;;;
;; general

(use-package general :ensure t)

(general-define-key :states '(normal visual insert emacs)
                    :prefix "SPC"
                    :non-normal-prefix "C-c"
                    :keymaps 'override

		    ":" 'helm-M-x

		    "b" '(:ignore t :which-key "buffers")
		    "bb" 'helm-buffers-list

		    "g" '(:ignore t :which-key "magit")
		    "gb" 'magit-blame
		    "gc" 'magit-clone
		    "gs" 'magit-status

		    "f" '(:ignore t :which-key "file")
		    "ff" '(helm-find-files :which-key "find file")
		    "fd" 'dired

		    "kb" 'kill-this-buffer
		    "kw" 'kill-buffer-and-window

		    "q" '(:ignore t :which-key "quit")
		    "qr" 'restart-emacs

		    "w" '(:ignore t :which-key "windows")
		    "wj" 'evil-window-down
		    "wk" 'evil-window-up
		    "wl" 'evil-window-right
		    "wh" 'evil-window-left
		    "wv" 'split-window-right
		    "wq" 'evil-quit
		    )

;;;;;;;;;;;;;;;;;;;
;; startup commands

(evil-mode)
(which-key-mode)
(company-mode)
(flycheck-mode)

;; turn off some default stuff that i hate
(tool-bar-mode 0)
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode 0))
(when (fboundp 'tabbar-mode)
  (tabbar-mode 0))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(general evil-collection evil-magit magit evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
