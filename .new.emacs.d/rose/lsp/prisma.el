(use-package prisma-mode
  :straight (prisma-mode
             :type git
             :host github
             :repo "pimeys/emacs-prisma-mode")
  :ensure t
  :config
  (add-hook 'prisma-mode-hook 'lsp))
