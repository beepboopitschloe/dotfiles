(use-package omnisharp :ensure t
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook #'rose/setup-c-indent)
  (eval-after-load
      'company
    (add-to-list 'company-backends 'company-omnisharp)))
