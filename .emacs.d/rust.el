(message "configuring rust...")

(use-package rust-mode :ensure t)

(use-package flycheck-rust :ensure t
  :config
  (add-hook 'rust-mode-hook #'flycheck-rust-setup))

(use-package racer :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))
