(message "configuring dlang...")

(defun rose/d-mode-hook ()
  (interactive)
  (setq indent-tabs-mode nil))

(defun rose/dub/dub (&optional command)
  (interactive)
  (let ((cmd (if command command "")))
    (rose/shell-command-in-project-root (format "dub %s" cmd))))

(defun rose/dub/build ()
  (interactive)
  (rose/dub/dub "build"))

(use-package d-mode :ensure t
  :config
  (add-hook 'd-mode-hook 'rose/d-mode-hook))

(use-package flycheck-dmd-dub :ensure t
  :config
  (add-hook 'd-mode-hook 'flycheck-dmd-dub-set-variables))

(use-package company-dcd :ensure t
  :config
  (add-hook 'd-mode-hook 'company-dcd-mode))

(general-define-key :states '(normal visual insert emacs)
                    :keymaps 'd-mode-map
                    :prefix "SPC"
                    :non-normal-prefix "C-c"

                    "md" '(:ignore t :which-key "dub")
                    "mdb" 'rose/dub/build
                    "mdd" 'rose/dub/dub)
