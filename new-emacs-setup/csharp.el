(use-package omnisharp :ensure t
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook #'nmuth/setup-c-indent)
  (eval-after-load
      'company
    (add-to-list 'company-backends 'company-omnisharp)))
