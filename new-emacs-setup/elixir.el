(message "configuring elixir...")

(defun nmuth/alchemist-hook ()
  (company-mode))

(defun nmuth/iex-hook ()
  (company-mode))

(use-package elixir-mode :ensure t)
(use-package alchemist :ensure t
  :config
  (add-hook 'alchemist-mode-hook #'nmuth/alchemist-hook)
  (add-hook 'alchemist-iex-mode-hook #'nmuth/iex-hook))

(general-define-key :states '(normal visual insert emacs)
                    :keymaps 'alchemist-mode-map
                    :prefix "SPC"
                    :non-normal-prefix "C-c"

                    "mi" '(:ignore t :which-key "Iex")
                    "mib" 'alchemist-iex-compile-this-buffer
                    
                    "mx" 'alchemist-mix)
