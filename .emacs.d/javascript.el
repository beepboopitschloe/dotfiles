(message "configuring javascript...")

(defun rose/web-mode-javascript-setup ()
  (interactive)
  (web-mode-set-content-type "jsx")
  ;(tide-setup)
  (lsp)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-javascript-eslint-executable "eslint_d")
  (eldoc-mode +1)
  (company-mode +1)
  (eslintd-fix-mode))

(defun rose/web-mode-hook ()
  (if (or (equal web-mode-content-type "javascript") (equal web-mode-content-type "jsx"))
      (rose/web-mode-javascript-setup)
    (message "set content type to %s" web-mode-content-type))
  (when (string-match "tsx?" (file-name-extension buffer-file-name))
    (rose/typescript-setup))
  (when (string-match "vue" (file-name-extension buffer-file-name))
    (rose/vue-setup))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 0)
  (setq web-mode-code-padding 0)
  ;; web mode sets its own comment face and it's fucking unreadable
  (set-face-foreground 'web-mode-javascript-comment-face "cyan")
  (set-face-foreground 'web-mode-css-comment-face "cyan")
  (setq indent-tabs-mode nil))

(defun rose/vue-setup ()
  (lsp)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-javascript-eslint-executable "eslint_d")
  (company-mode +1)
  (eslintd-fix-mode))

(defun rose/typescript-setup ()
  (interactive)
  ;(tide-setup)
  (lsp)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-javascript-eslint-executable "eslint_d")
  (eldoc-mode +1)
  (company-mode +1)
  (eslintd-fix-mode))

(defun rose/eslint-fix ()
  (interactive)
  (shell-command (concat "yarn eslint --fix " (buffer-file-name))))

(use-package tide :ensure t
  :config
  (add-hook 'typescript-mode-hook #'rose/typescript-setup))

(use-package web-mode :ensure t
  :config
  (setq web-mode-enable-auto-closing t)

  (add-hook 'web-mode-hook 'rose/web-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[s]?css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

(defun rose/less-css-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq css-indent-offset 2))

(use-package less-css-mode :ensure t
  :config
  (add-hook 'less-css-mode-hook 'rose/less-css-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode)))

;; npm-mode
;; TODO: npm-mode-npm-run doesn't seem to reuse its shell buffer, either need to
;; submit a PR or write my own client
(use-package npm-mode :ensure t)

(use-package eslintd-fix :ensure t)

(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))

(general-define-key :states '(normal visual insert emacs)
                    :keymaps 'web-mode-map
                    :prefix "SPC"
                    :non-normal-prefix "C-c"

                    "mf" 'rose/eslint-fix

                    ;"md" 'tide-jump-to-definition
                    ;"mi" 'tide-jump-to-implementation
                    ;"mr" 'tide-references
                    ;"mR" 'tide-rename-symbol

                    "md" 'lsp-find-definition
                    "mD" 'lsp-find-type-definition
                    "mi" 'lsp-goto-implementation
                    "mr" 'lsp-find-references
                    "mR" 'lsp-rename
                    "mS" 'lsp-restart-workspace

                    "mn" '(:ignore t :which-key "npm")
                    "mnd" 'npm-mode-npm-install-save-dev
                    "mni" 'npm-mode-npm-install-save
                    "mnl" 'npm-mode-npm-list
                    "mnn" 'npm-mode-npm-inig
                    "mnr" 'npm-mode-npm-run
                    "mnu" 'npm-mode-npm-uninstall
                    "mnv" 'npm-mode-visit-project-file)


(general-define-key :states '(normal visual insert emacs)
                    :keymaps 'tide-references-mode-map

                    "RET" 'tide-goto-reference
                    "n" 'tide-find-next-reference
                    "p" 'tide-find-previous-reference
                    "q" 'quit-window)
