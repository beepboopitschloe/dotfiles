(message "configuring pony...")

(defun rose/ponylang-mode-hook ()
  (interactive)
  (set-variable 'indent-tabs-mode nil)
  (set-variable 'tab-width 2)
  (company-mode 0))

(use-package ponylang-mode :ensure t
  :config
  (add-hook 'ponylang-mode-hook 'rose/ponylang-mode-hook))
