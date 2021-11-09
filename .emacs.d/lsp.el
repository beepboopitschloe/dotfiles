(message "configuring LSP...")

(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-l")
  :hook ((dart-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui :ensure t)
(use-package company-lsp :ensure t)

;; wip
'(general-define-key :states '(normal visual insert emacs)
                     :keymaps 'lsp-mode-map
                     :prefix "SPC"
                     :non-normal-prefix "C-c"

                     "j=" 'lsp-format-buffer

                     "lss" 'lsp
                     "lsr" 'lsp-workspace-restart
                     "lsq" 'lsp-workspace-shutdown
                     "lsd" 'lsp-describe-session
                     "lsD" 'lsp-disconnect)
