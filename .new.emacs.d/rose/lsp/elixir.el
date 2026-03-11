(defun rose/elixir-setup ()
  (lsp))

(use-package alchemist
  :straight t
  :ensure t
  :config
  (add-hook 'alchemist-mode-hook 'rose/elixir-setup))

