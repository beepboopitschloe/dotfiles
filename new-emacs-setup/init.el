;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sensible defaults from:
;;
;; https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/

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
(setq default-fill-column 80)
(setq create-lockfiles nil)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; some utility functions

(defun nmuth/light-theme ()
  (interactive)
  (unless (boundp 'color-theme-initialized)
    (color-theme-initialize))
  (color-theme-feng-shui))

(defun nmuth/dark-theme ()
  (interactive)
  (unless (boundp 'color-theme-initialized)
    (color-theme-initialize))
  (color-theme-charcoal-black)) ; (color-theme-jonadabian-slate))

(defun nmuth/gui-setup ()
  (nmuth/dark-theme)
  (exec-path-from-shell-initialize))

(defun nmuth/toggle-indent-tabs-mode ()
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "tabs: %s" (if indent-tabs-mode "yes" "no")))

(defun current-major-mode ()
  (interactive)
  (message "current major mode is %s" major-mode))

(defun nmuth/first-load-setup ()
  ;; configure GUI things if we're running in a GUI
  (when (memq window-system '(mac ns))
    (nmuth/gui-setup))
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

(use-package evil :ensure t)
(use-package magit :ensure t)
(use-package evil-magit :ensure t)
(use-package shell-pop :ensure t)
(use-package linum-relative :ensure t)
(use-package flx-ido :ensure t)
(use-package smart-tabs-mode :ensure t)
(use-package exec-path-from-shell :ensure t)
(use-package restart-emacs :ensure t)
(use-package swiper :ensure t)
(use-package counsel :ensure t)
(use-package counsel-projectile :ensure t)
(use-package ag :ensure t)
(use-package restclient :ensure t)
(use-package ledger-mode :ensure t)
(use-package elixir-mode :ensure t)
(use-package alchemist :ensure t)
(use-package lua-mode :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages that require configuration

(use-package company :ensure t
  :config
  (setq company-idle-delay 0.1))

(use-package which-key :ensure t
  :config
  (setq which-key-idle-delay 0.25))

(use-package ivy :ensure t
  :config
  (setq ivy-height 15))

(use-package projectile :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'projectile-dired))

;;;;;;;;;;;;;;;
;; key bindings

(use-package general :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-c"

   "TAB" 'spacemacs/alternate-buffer
   ":" 'counsel-M-x

   "a" '(:ignore t :which-key "applications")
   "as" '(:ignore t :which-key "shells")
   "ast" 'shell-pop

   "b" '(:ignore t :which-key "buffer")
   "bb" 'ido-switch-buffer
   "bd" 'kill-this-buffer
   "be" 'eval-buffer
   "bn" 'next-buffer
   "bp" 'previous-buffer
   "br" 'rename-buffer

   "f" '(:ignore t :which-key "file")
   "ff" '(counsel-find-file :which-key "find file")
   "fr" 'counsel-recentf

   "g" '(:ignore t :which-key "magit")
   "gc" 'magit-clone
   "gs" 'magit-status

   "h" '(:ignore t :which-key "help")
   "hdf" 'counsel-describe-function
   "hdv" 'counsel-describe-variable

   "j=" 'spacemacs/indent-region-or-buffer

   "l" 'mac-launchpad

   "o" '(:ignore t :which-key "org")
   "oa" 'org-agenda
   "oo" (lambda () (interactive) (find-file "~/org/index.org"))
   "oO" 'org-clock-out

   "p" '(:ignore t :which-key "projectile")
   "pa" 'projectile-ag
   "pb" 'counsel-projectile-switch-to-buffer
   "pd" 'counsel-projectile-find-dir
   "pf" 'counsel-projectile-find-file
   "pp" 'counsel-projectile-switch-project

   "t" '(:ignore t :which-key "misc")
   "tl" 'linum-mode
   "tw" 'whitespace-mode
   "t TAB" 'nmuth/toggle-indent-tabs-mode
   "tt" '(:ignore t :which-key "themes")
   "ttl" 'nmuth/light-theme
   "ttd" 'nmuth/dark-theme

   "q" '(:ignore t :which-key "quit")
   "qr" 'restart-emacs

   "w" '(:ignore t :which-key "windows")
   "wj" 'evil-window-down
   "wk" 'evil-window-up
   "wl" 'evil-window-right
   "wh" 'evil-window-left
   "wv" 'split-window-right
   "wV" 'spacemacs/split-window-right-and-focus
   "wc" 'close-window
   "wm" 'spacemacs/toggle-maximize-buffer
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; specific mode configuration

(load-file "~/.emacs.d/dlang.el")
(load-file "~/.emacs.d/javascript.el")
(load-file "~/.emacs.d/org.el")
(load-file "~/.emacs.d/mac-launchpad.el")
(load-file "~/.emacs.d/project-shell-commands.el")

;;;;;;;;;;;;;;;;;;;;;;
;; final startup tasks

;; turn on some minor modes
(evil-mode)
(ivy-mode)
(which-key-mode)
(linum-relative-global-mode)
(projectile-global-mode +1)
(tool-bar-mode 0)
(tabbar-mode 0)
(scroll-bar-mode 0)

;; add indent-tabs-mode = nil as a default hook for all files. add it to the end
;; of the list so that we can easily override it for particular modes. (add-hook
(add-hook 'after-load-functions (lambda (f) (setq indent-tabs-mode nil)) t)

;; open an org file if we're starting emacs now
(unless (boundp 'nmuth/first-startup-finished)
  (nmuth/first-load-setup))
(defvar nmuth/first-startup-finished t)
