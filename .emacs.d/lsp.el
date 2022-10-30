(message "configuring LSP...")

(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-l")
  :hook ((dart-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui :ensure t)
(use-package company-lsp :ensure t)

;;  prisma
(use-package quelpa-use-package
  :ensure t
  :init (setq quelpa-update-melpa-p nil)
  :config (quelpa-use-package-activate-advice))

(use-package prisma-mode
  :ensure t
  :quelpa (eg
           :fetcher github
           :repo "pimeys/emacs-prisma-mode")
  :config
  (add-hook 'prisma-mode-hook 'lsp))

(with-eval-after-load 'lsp-volar
  (setq lsp-typescript-tsdk (file-name-directory (lsp-volar-get-typescript-server-path))))

(advice-add 'json-parse-string :around 
            (lambda (orig string &rest rest)
              (apply orig (s-replace "\\u0000" "" string)
                     rest)))

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
