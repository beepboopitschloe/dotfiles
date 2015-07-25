(add-to-list 'load-path "~/Misc/slime/")
(require 'slime-autoloads)
(setq inferior-lisp-program "~/Misc/ccl/dx86cl64")

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))
(require 'package)
(package-initialize)

(require 'elisp-slime-nav)


(defun my-lisp-hook ()
  (elisp-slime-nav-mode)
  (turn-on-eldoc-mode))
(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)

; trigger word combos
(defvar word-cmd-plist ())

(defmacro do-word-command (word)
  `(funcall (lax-plist-get word-cmd-plist ,word)))
(defmacro cmd-as-lambda (&rest cmd)
  `(lambda () ,@cmd))
(defmacro add-word-cmd (word &rest cmd)
  `(setq word-cmd-plist
	 (lax-plist-put word-cmd-plist
			,word
			,(macroexpand `(cmd-as-lambda ,@cmd)))))

(defun get-word-cmd (word)
  (lax-plist-get word-cmd-plist word))
(defun query-word-combo (x)
  (interactive "sCommand: ")
  (do-word-command x))

(add-word-cmd "m" (message "hello world"))
(add-word-cmd "dp" (interactive) (dired "~/Projects"))
(add-word-cmd "osl" (org-store-link))
(add-word-cmd "oil" (org-insert-link))
(add-word-cmd "oa" (org-agenda))
(add-word-cmd "ob" (org-iswitchb))
(add-word-cmd "ot" (org-insert-todo-heading))

(global-set-key [8388656] (quote query-word-combo))

; agenda settings
(setq org-agenda-files '("~/Projects/notes/work.org"))

; activate some plugins
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(global-relative-line-numbers-mode)

; highlight column 80
(require 'column-enforce-mode)
(80-column-rule)

; theme
(load-theme 'material)

; leader key :D
(require 'evil-leader)
(global-evil-leader-mode)

; org mode bindings
(add-hook 'org-mode-hook 'turn-on-font-lock)
(evil-leader/set-key
  "l" 'org-store-link
  "a" 'org-agenda
  "b" 'org-iswitchb
  "d" '(lambda()
	 (interactive)
	 (dired "~/Projects")))

; vim o7
(require 'evil)
(evil-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("9e7cc7f1db252d6fe0c7cc25d72d768712a97cda1f77bf63f0c1fd7be5dee7f4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
