(message "configuring elm...")

(defun rose/elm-mode-hook ()
  (interactive)
  (elm-oracle-setup-completion)
  (setq company-backend 'company-elm))

(use-package elm-mode :ensure t
  :config
  (add-to-list 'company-backends 'company-elm)
  (add-hook 'elm-mode-hook #'rose/elm-mode-hook)
  (setq elm-format-on-save t))

(use-package flycheck-elm :ensure t
  :config
  (shell-command "yarn global add elm-oracle"))

(general-define-key :states '(normal visual insert emacs)
                    :keymaps 'elm-mode-map
                    :prefix "SPC"
                    :non-normal-prefix "C-c"

                    "j =" 'elm-mode-format-buffer)
