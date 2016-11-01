(message "configuring elm...")

(defun nmuth/elm-mode-hook ()
  (elm-oracle-setup-completion)
  (setq company-backend 'company-elm))

(use-package elm-mode :ensure t
  :config
  (add-to-list 'company-backends 'company-elm)
  (add-hook 'elm-mode-hook #'nmuth/elm-mode-hook))

(use-package flycheck-elm :ensure t
  :config
  (shell-command "yarn global add elm-oracle"))
