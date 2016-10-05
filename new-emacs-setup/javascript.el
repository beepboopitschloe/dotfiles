(message "configuring javascript...")

(defun nmuth/web-mode-hook ()
  (if (equal web-mode-content-type "javascript")
      (progn
        (web-mode-set-content-type "jsx")
        (npm-mode))
    (message "set content type to %s" web-mode-content-type))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package web-mode :ensure t
  :config
  (add-hook 'web-mode-hook 'nmuth/web-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

;; npm-mode
;; TODO: npm-mode-npm-run doesn't seem to reuse its shell buffer, might have to
;; write my own client...
(use-package npm-mode :ensure t)

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
                    "mnv" 'npm-mode-visit-project-file)