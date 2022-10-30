;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up use-package before anything else

(load-file "~/.emacs.d/rose/package.el")
(load-file "~/.emacs.d/rose/lsp.el")

;;;;;;;;;;;;;;;;;;;
;; custom functions

(defun rose/switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

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
(use-package projectile :ensure t
  :config
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-mode-line '(:eval (format " Projectile[%s]" (projectile-project-name)))))
(use-package helm-projectile :straight t :ensure t)

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

		    "TAB" 'rose/switch-to-other-buffer
		    ":" 'helm-M-x

		    "b" '(:ignore t :which-key "buffers")
		    "bb" 'helm-buffers-list
		    "be" 'eval-buffer

		    "g" '(:ignore t :which-key "magit")
		    "gb" 'magit-blame
		    "gc" 'magit-clone
		    "gs" 'magit-status

		    "f" '(:ignore t :which-key "file")
		    "ff" '(helm-find-files :which-key "find file")
		    "fd" 'dired

		    "kb" 'kill-this-buffer
		    "kw" 'kill-buffer-and-window

		    "p" '(:ignore t :which-key "projectile")
		    "pa" 'helm-projectile-ag
		    "pb" 'helm-projectile-switch-to-buffer
		    "pd" 'helm-projectile-find-dir
		    "pf" 'helm-projectile
		    "pp" 'helm-projectile-switch-project

		    "q" '(:ignore t :which-key "quit")
		    "qq" 'kill-emacs

		    "w" '(:ignore t :which-key "windows")
		    "wj" 'evil-window-down
		    "wk" 'evil-window-up
		    "wl" 'evil-window-right
		    "wh" 'evil-window-left
		    "wv" 'split-window-right
		    "wq" 'evil-quit
		    )

;;;;;;;;;;;;;;;;
;; last few vars

(setq split-width-threshold 0)
(setq split-height-threshold nil)

;;;;;;;;;;;;;;;;;;;
;; startup commands

(evil-mode)
(which-key-mode)
(projectile-global-mode +1)
(global-company-mode)
(global-flycheck-mode)

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
