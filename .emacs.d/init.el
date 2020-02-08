;;; package --- init.el
;;; Commentary:
;;; Code:

;; sensible defaults from https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/

;; this function is called in package.el but the string has to be present in init.el
;; (package-initialize)

(add-to-list 'load-path "~/.emacs.d/vendor/swiper/")
(add-to-list 'load-path "~/.emacs.d/vendor/counsel-projectile/")

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq vc-follow-symlinks t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)
(setq create-lockfiles nil)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(setq-default fill-column 80)

;;;;;;;;;;;;;;;;;;;;
;; C indent style

(defun nmuth/setup-c-indent ()
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'statement-cont '-))

(setq-default c-basic-offset 2)
(add-hook 'c-mode-common-hook #'nmuth/setup-c-indent)

;;;;;;;;;;;;;;;;;;;;;;;
;; make copy/paste work

(defun nmuth/pbcopy ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "reattach-to-user-namespace pbcopy"))

(defun nmuth/pbcut ()
  (interactive)
  (nmuth/pbcopy)
  (delete-region (region-beginning) (region-end)))

(defun nmuth/pbpaste ()
  (interactive)
  (let ((paste (shell-command-to-string "reattach-to-user-namespace pbpaste")))
    (when mark-active (evil-delete (point) (mark)))
    (insert paste)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; some utility functions

(defun nmuth/light-theme ()
  "Enable light GUI theme."
  (interactive)
  (progn
    (color-theme-initialize)
    (color-theme-feng-shui)))

(defun nmuth/dark-theme ()
  "Enable dark GUI theme."
  (interactive)
  (progn
    (load-theme 'srcery t)
    (set-face-foreground 'font-lock-comment-face "cyan")
    ;; (color-theme-initialize)
    ;; (color-theme-charcoal-black)
    )) ; (color-theme-jonadabian-slate))

(defun nmuth/gui-setup ()
  "Perform GUI-specific setup."
  (exec-path-from-shell-initialize))

(defun nmuth/toggle-indent-tabs-mode ()
  "Toggle whether or not to indent with tabs."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "tabs: %s" (if indent-tabs-mode "yes" "no")))

(defun current-major-mode ()
  "Check the current major mode."
  (interactive)
  (message "current major mode is %s" major-mode))

(defun nmuth/first-load-setup ()
  "Perform first-load setup functions."
  (when (memq window-system '(mac ns))
    (nmuth/gui-setup)) ; set up GUI things if we're running in a GUI
  (find-file "~/org/index.org"))

;; portable 'close-window
(unless (boundp 'close-window)
  (defun close-window () (interactive) (delete-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load necessary functions for configuration

(load-file "~/.emacs.d/spacemacs-funcs.el")
(load-file "~/.emacs.d/package.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages that don't require configuration

(use-package persistent-scratch :ensure t)
(use-package evil :ensure t)
(use-package evil-escape :ensure t)
(use-package evil-collection :ensure t
             :config
             (setq evil-want-keybinding nil)
             (evil-collection-init))
(use-package magit :ensure t)
(use-package evil-magit :ensure t)
(use-package shell-pop :ensure t)
(use-package linum-relative :ensure t)
(use-package smart-tabs-mode :ensure t)
(use-package exec-path-from-shell :ensure t)
(use-package restart-emacs :ensure t)
(use-package ag :ensure t)
(use-package restclient :ensure t)
(use-package ledger-mode :ensure t)
(use-package lua-mode :ensure t)
(use-package flycheck :ensure t)
(use-package yaml-mode :ensure t)
(use-package erc :ensure t)
(use-package color-theme :ensure t)
(use-package nord-theme :ensure t)
(use-package srcery-theme :ensure t)
(use-package markdown-mode :ensure t)
(use-package haskell-mode :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages that require configuration

(use-package company :ensure t
  :config
  (setq company-idle-delay 0.1)
  (setq company-global-modes '(not org-mode fundamental-mode text-mode)))

(use-package which-key :ensure t
  :config
  (setq which-key-idle-delay 0.25))

(use-package projectile :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-mode-line '(:eval (format " Projectile[%s]" (projectile-project-name)))))

(use-package emojify :ensure t
  :config
  (setq emojify-emoji-styles '(unicode))
  (setq emojify-display-style 'unicode)
  (global-emojify-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages in the vendor directory

(load-library "swiper")

(load-library "ivy")
(setq ivy-height 15)

(load-library "counsel")

(load-library "counsel-projectile")

;;;;;;;;;;;;;;;
;; key bindings

(use-package general :ensure t)

;; override bindings (available everywhere)
(general-define-key :states '(normal visual insert emacs)
                    :prefix "SPC"
                    :non-normal-prefix "C-c"
                    :keymaps 'override

		    "TAB" 'spacemacs/alternate-buffer
		    ":" 'counsel-M-x

		    "a" '(:ignore t :which-key "ag")
		    "aa" 'ag

		    "b" '(:ignore t :which-key "buffer")
		    "bb" 'ivy-switch-buffer
		    "bd" 'kill-this-buffer
		    "be" 'eval-buffer
		    "bn" 'next-buffer
		    "bp" 'previous-buffer
		    "br" 'rename-buffer

		    "f" '(:ignore t :which-key "file")
		    "ff" '(counsel-find-file :which-key "find file")
		    "fr" 'counsel-recentf

		    "g" '(:ignore t :which-key "magit")
		    "gb" 'magit-blame
		    "gc" 'magit-clone
		    "gs" 'magit-status

		    "h" '(:ignore t :which-key "help")
		    "hdf" 'counsel-describe-function
		    "hdv" 'counsel-describe-variable

		    "kb" 'kill-this-buffer
		    "kw" 'kill-buffer-and-window

		    "p" '(:ignore t :which-key "projectile")
		    "pa" 'projectile-ag
		    "pb" 'counsel-projectile-switch-to-buffer
		    "pd" 'counsel-projectile-find-dir
		    "pf" 'counsel-projectile-find-file
		    "pp" 'counsel-projectile-switch-project

		    "q" '(:ignore t :which-key "quit")
		    "qr" 'restart-emacs

		    "w" '(:ignore t :which-key "windows")
		    "wj" 'evil-window-down
		    "wk" 'evil-window-up
		    "wl" 'evil-window-right
		    "wh" 'evil-window-left
		    "wv" 'split-window-right
		    "wV" 'spacemacs/split-window-right-and-focus
		    "w-" 'spacemacs/split-window-below-and-focus
		    "w|" 'spacemacs/split-window-right-and-focus
		    "wc" 'close-window
		    "wm" 'spacemacs/toggle-maximize-buffer)
 
;; defaults that _will_ be overridden by local keymaps (most annoyingly, dired)
(general-define-key :states '(normal visual insert emacs)
		    :prefix "SPC"
		    :non-normal-prefix "C-c"

		    "X" 'nmuth/pbcut
		    "C" 'nmuth/pbcopy
		    "V" 'nmuth/pbpaste

		    "a" '(:ignore t :which-key "applications")
		    "as" '(:ignore t :which-key "shells")
		    "ast" 'shell-pop

		    "e" '(:ignore t :which-key "emoji")
		    "ei" 'emojify-insert-emoji

		    "j=" 'spacemacs/indent-region-or-buffer

		    "l" 'mac-launchpad

		    "t" '(:ignore t :which-key "misc")
		    "tl" 'linum-mode
		    "tw" 'whitespace-mode
		    "t TAB" 'nmuth/toggle-indent-tabs-mode
		    "tt" '(:ignore t :which-key "themes")
		    "ttl" 'nmuth/light-theme
		    "ttd" 'nmuth/dark-theme)

(general-define-key :states '(visual insert)
		    "ESC" 'evil-escape)

(general-define-key :states '(visual normal insert emacs)
                    "≈" 'counsel-M-x)

(global-set-key (kbd "<A-backspace>") 'evil-delete-backward-word)

(global-set-key (kbd "C-ç") 'calc)
(global-set-key (kbd "C-c C-k C-w") 'kill-buffer-and-window)

;; why does this error?
;;(general-define-key :states '(normal visual insert emacs)
;;"M-x" 'counsel-M-x)

;;  web-mode creates its own faces for some stupid reason, so we have to enable the theme before it loads
(nmuth/dark-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; specific mode configuration

(load-file "~/.emacs.d/dlang.el")
(load-file "~/.emacs.d/elm.el")
(load-file "~/.emacs.d/go.el")
(load-file "~/.emacs.d/javascript.el")
(load-file "~/.emacs.d/lisp.el")
(load-file "~/.emacs.d/nim.el")
(load-file "~/.emacs.d/mac-launchpad.el")
(load-file "~/.emacs.d/org.el")
(load-file "~/.emacs.d/python.el")
(load-file "~/.emacs.d/rust.el")
(load-file "~/.emacs.d/csharp.el")
(load-file "~/.emacs.d/project-shell-commands.el")
(load-file "~/.emacs.d/pony.el")

(use-package clojure-mode :ensure t)
(use-package cider :ensure t)

;;;;;;;;;;;;;;;;;;;;;;
;; final startup tasks

(persistent-scratch-setup-default)

;; turn on some minor modes
(evil-mode)
(ivy-mode)
(which-key-mode)
(linum-relative-global-mode 0)
(projectile-global-mode +1)
(tool-bar-mode 0)
(global-company-mode)
(global-flycheck-mode)
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode 0))
(when (fboundp 'tabbar-mode)
  (tabbar-mode 0))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

;; add indent-tabs-mode = nil as a default hook for all files. add it to the end
;; of the list so that we can easily override it for particular modes.
(add-hook 'after-load-functions (lambda (f) (setq indent-tabs-mode nil)) t)

;; open an org file if we're starting emacs now
(unless (boundp 'nmuth/first-startup-finished)
  (nmuth/first-load-setup))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((((type tty)) (:foreground "green"))))
 '(magit-diff-added-highlight ((((type tty)) (:foreground "LimeGreen"))))
 '(magit-diff-context-highlight ((((type tty)) (:foreground "default"))))
 '(magit-diff-file-heading ((((type tty)) nil)))
 '(magit-diff-removed ((((type tty)) (:foreground "red"))))
 '(magit-diff-removed-highlight ((((type tty)) (:foreground "IndianRed"))))
 '(magit-section-highlight ((((type tty)) nil))))

(defvar nmuth/first-startup-finished t)
