(use-package go-mode
  :straight t
  :ensure t
  :config
  (add-hook 'go-mode-hook 'lsp-deferred))
