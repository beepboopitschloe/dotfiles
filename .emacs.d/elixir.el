(message "configuring elixir...")

(defun rose/alchemist-hook ()
  (company-mode))

(defun rose/iex-hook ()
  (company-mode))

(use-package elixir-mode :ensure t)
(use-package alchemist :ensure t
  :config
  (add-hook 'elixir-mode-hook (lambda () (alchemist-mode)))
  (add-hook 'alchemist-mode-hook #'rose/alchemist-hook)
  (add-hook 'alchemist-iex-mode-hook #'rose/iex-hook))

(general-define-key :states '(normal visual insert emacs)
		    :keymaps 'alchemist-mode-map
		    :prefix "SPC"
		    :non-normal-prefix "C-c"

		    "mi" '(:ignore t :which-key "Iex")
		    "mib" 'alchemist-iex-compile-this-buffer

		    "mx" 'alchemist-mix)

;;;;;;;;;
;; erlang

(defun rose/load-erlang ()
  (interactive)
  (let* ((erl-path "/opt/homebrew/Cellar/erlang/24.3.4/lib/erlang/")
	 (erl-emacs-path (concat erl-path "lib/tools-3.5.2/emacs/"))
	 (erl-exec-path (concat erl-path "bin")))
    (setq load-path (cons erl-emacs-path load-path))
    (require 'erlang-start)
    (setq erlang-root-dir erl-path)
    (setq exec-path (cons erl-exec-path exec-path))
    (setq erlang-man-root-dir (concat erl-path "man"))))

(rose/load-erlang)
