(defun rose/js-lsp-setup ()
  (lsp)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-javascript-eslint-executable "eslint_d")
  (company-mode +1)
  (eslintd-fix-mode))

(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . js-mode))
(add-hook 'js-mode-hook 'rose/js-lsp-setup)

;;;;;;;;;;;;;;;;
;; install stuff

(use-package eslintd-fix :straight t :ensure t)
(use-package vue-mode
  :straight t
  :ensure t
  :config
  (add-hook 'vue-mode-hook 'rose/js-lsp-setup))
