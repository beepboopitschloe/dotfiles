(message "configuring javascript...")

(defun nmuth/web-mode-javascript-setup ()
  (interactive)
  (web-mode-set-content-type "jsx")
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq web-mode-code-indent-offset 2)
  (eldoc-mode +1)
  (company-mode +1))

(defun nmuth/web-mode-hook ()
  (if (equal web-mode-content-type "javascript")
      (nmuth/web-mode-javascript-setup)
    (message "set content type to %s" web-mode-content-type))
  (when (string-match "tsx?" (file-name-extension buffer-file-name))
    (nmuth/typescript-setup))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  ;; web mode sets its own comment face and it's fucking unreadable
  (set-face-foreground 'web-mode-javascript-comment-face "cyan")
  (set-face-foreground 'web-mode-css-comment-face "cyan")
  (setq indent-tabs-mode nil))

(defun nmuth/typescript-setup ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (company-mode +1))

(use-package tide :ensure t
  :config
  (add-hook 'typescript-mode-hook #'nmuth/typescript-setup))

(use-package web-mode :ensure t
  :config
  (add-hook 'web-mode-hook 'nmuth/web-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[s]?css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

(defun nmuth/less-css-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq css-indent-offset 2))

(use-package less-css-mode :ensure t
  :config
  (add-hook 'less-css-mode-hook 'nmuth/less-css-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode)))

;; npm-mode
;; TODO: npm-mode-npm-run doesn't seem to reuse its shell buffer, either need to
;; submit a PR or write my own client
(use-package npm-mode :ensure t)

(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))

(general-define-key :states '(normal visual insert emacs)
                    :keymaps 'web-mode-map
                    :prefix "SPC"
                    :non-normal-prefix "C-c"

                    "mn" '(:ignore t :which-key "npm")
                    "mnd" 'npm-mode-npm-install-save-dev
                    "mni" 'npm-mode-npm-install-save
                    "mnl" 'npm-mode-npm-list
                    "mnn" 'npm-mode-npm-inig
                    "mnr" 'npm-mode-npm-run
                    "mnu" 'npm-mode-npm-uninstall
                    "mnv" 'npm-mode-visit-project-file

                    "md" 'tide-jump-to-definition
                    "mi" 'tide-jump-to-implementation
                    "mr" 'tide-references
                    "mR" 'tide-rename-symbol

                    "mS" 'tide-restart-server)


(general-define-key :states '(normal visual insert emacs)
                    :keymaps 'tide-references-mode-map

                    "RET" 'tide-goto-reference
                    "n" 'tide-find-next-reference
                    "p" 'tide-find-previous-reference
                    "q" 'quit-window)
