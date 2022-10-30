(defun rose/hurl ()
  (interactive)
  (let ((vars-file "vars.env")
        (hurl-file buffer-file-name))
    (shell-command
     (format "hurl --variables-file ~/Projects/instil/requests/%s %s | python -m json.tool" vars-file hurl-file))))

(defun rose/hurl-run ()
  (interactive)
  (counsel-find-file "~/Projects/instil/requests/")
  (rose/hurl))

(defun rose/hurl-goto-home ()
  (interactive)
  (find-file "~/Projects/instil/requests"))

(general-define-key :states '(normal visual insert emacs)
                    :prefix "C-c"
                    :non-normal-prefix "C-c"
                    
                    "hh" 'rose/hurl
                    "hH" 'rose/hurl-goto-home
                    "hr" 'rose/hurl-run)
