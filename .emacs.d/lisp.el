;;; rose/lisp --- Lisp configuration
;;; Commentary:

;;; Code:

;; https://github.com/roswell/roswell/wiki/1.1-Initial-Recommended-Setup

; (load (expand-file-name "~/.roswell/helper.el"))
; (load (expand-file-name "~/.roswell/lisp/quicklisp/slime-helper.el"))

(setq inferior-lisp-program "ros -Q run")

(setf slime-lisp-implementations
      `((roswell ("ros" "-Q" "run"))))
(setf slime-default-lisp 'roswell)

(setq-default rose/lisp-modes '(clojure-mode
				 clojurescript-mode
				 emacs-lisp-mode
				 lisp-mode))

(defun rose/activate-lispy-if-lisp ()
  "Activate Lispy f the current mode is one of rose/lisp-modes."
  (interactive)
  (message "look at me")
  (if (member major-mode rose/lisp-modes)
      (lispy-mode 1)))

(defun rose/deactivate-lispy ()
  (lispy-mode 0))

(use-package slime :ensure t
  :config
  (setq slime-contribs '(slime-fancy slime-company)))
(use-package slime-company :ensure t)

(use-package lispy :ensure t
  :config
  (add-hook 'evil-insert-state-entry-hook #'rose/activate-lispy-if-lisp)
  (add-hook 'evil-insert-state-exit-hook #'rose/deactivate-lispy))
