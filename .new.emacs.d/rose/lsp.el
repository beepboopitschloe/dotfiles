(message "configuring LSP...")

(use-package flycheck :straight t :ensure t)
(use-package lsp-mode
  :straight t
  :ensure t
  :init (setq lsp-keymap-prefix "C-l")
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui :straight t :ensure t :commands lsp-ui-mode)
(use-package helm-lsp :straight t :ensure t :commands helm-lsp-workspace-symbol)

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

;;;;;;;;;;;;;
;; dependents

(load-file "~/.emacs.d/rose/lsp/javascript.el")
