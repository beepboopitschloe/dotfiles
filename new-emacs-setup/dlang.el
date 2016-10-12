(message "configuring dlang...")

(defun nmuth/d-mode-hook ()
  (interactive)
  (setq indent-tabs-mode nil))

(defun nmuth/dub/dub (&optional command)
  (interactive)
  (let ((cmd (if command command "")))
    (nmuth/shell-command-in-project-root (format "dub %s" cmd))))

(defun nmuth/dub/build ()
  (interactive)
  (nmuth/dub/dub "build"))

(use-package d-mode :ensure t
  :config
  (add-hook 'd-mode-hook #'nmuth/d-mode-hook))

(general-define-key :states '(normal visual insert emacs)
                    :keymaps 'd-mode-map
                    :prefix "SPC"
                    :non-normal-prefix "C-c"

                    "md" '(:ignore t :which-key "dub")
                    "mdb" 'nmuth/dub/build
                    "mdd" 'nmuth/dub/dub)
