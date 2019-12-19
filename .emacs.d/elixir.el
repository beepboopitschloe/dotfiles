(message "configuring elixir...")

(defun nmuth/alchemist-hook ()
  (company-mode))

(defun nmuth/iex-hook ()
  (company-mode))

(use-package elixir-mode :ensure t)
(use-package alchemist :ensure t
  :config
  (add-hook 'elixir-mode-hook (lambda () (alchemist-mode)))
  (add-hook 'alchemist-mode-hook #'nmuth/alchemist-hook)
  (add-hook 'alchemist-iex-mode-hook #'nmuth/iex-hook))

(general-define-key :states '(normal visual insert emacs)
		    :keymaps 'alchemist-mode-map
		    :prefix "SPC"
		    :non-normal-prefix "C-c"

		    "mi" '(:ignore t :which-key "Iex")
		    "mib" 'alchemist-iex-compile-this-buffer

		    "mx" 'alchemist-mix)

;;;;;;;;;
;; erlang

(defun nmuth/load-erlang ()
  (interactive)
  (let* ((erl-path "/usr/local/Cellar/erlang/22.1.8//lib/erlang/")
	 (erl-emacs-path (concat erl-path "lib/tools-3.2.1/emacs/"))
	 (erl-exec-path (concat erl-path "bin")))
    (setq load-path (cons erl-emacs-path load-path))
    (require 'erlang-start)
    (setq erlang-root-dir erl-path)
    (setq exec-path (cons erl-exec-path exec-path))
    (setq erlang-man-root-dir (concat erl-path "man"))))

(nmuth/load-erlang)
