(message "configuring python...")

(defun nmuth/python-mode-hook ()
  )

(use-package anaconda-mode :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

(general-define-key :states '(normal visual insert emacs)
                    :keymaps 'd-mode-map
                    :prefix "SPC"
                    :non-normal-prefix "C-c"

		    )
