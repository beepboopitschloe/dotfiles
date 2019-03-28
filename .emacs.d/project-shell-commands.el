(message "configuring project shell commands...")

(defun nmuth/term-buffer-name (project)
  (format "*%s-term*" project))

(defun nmuth/spawn-term (name &optional directory)
  "Create a shell buffer with the given name"
  (interactive "MName of shell buffer to create: ")
  (let ((buf (get-buffer-create (generate-new-buffer-name name))))
    (shell buf)
    (process-send-string buf ". ~/.bash_profile\n")
    (when directory
      (process-send-string buf (format "cd %s\n" directory)))
    (process-send-string buf "echo ''\n")
    buf))

(defun nmuth/project-info (&optional explicit-project)
  (let* ((project-root (projectile-project-root))
					;(project-path-parts (if project-root
					;(split-string project-root "/")
					;(error (format "bad project def: %S" project))))
	 (project-name (projectile-project-name))
	 (term-buffer-name (nmuth/term-buffer-name project-name)))
    (list 'name project-name 'root project-root 'term-buffer-name term-buffer-name)))

(defun nmuth/find-or-open-shell-for-current-project ()
  (interactive)
  (let* ((project (nmuth/project-info))
	 (project-name (plist-get project 'name))
	 (project-root (plist-get project 'root))
	 (buffer-name (plist-get project 'term-buffer-name))
	 (existing-term-buffer (get-buffer buffer-name)))
    (if existing-term-buffer
	(progn
	  (process-send-string existing-term-buffer (format "cd %s\n" project-root))
	  (process-send-string existing-term-buffer "echo ''\n")
	  (pop-to-buffer existing-term-buffer))
      (progn
	(pop-to-buffer (nmuth/spawn-term buffer-name project-root))
	(cd project-root)))))

(defun nmuth/project-shell-cd-to-root (&optional project-info)
  (interactive)
  (let* ((project (if project-info
		      project-info
		    (nmuth/project-info)))
	 (root (plist-get project 'root))
	 (buffer-name (plist-get project 'term-buffer-name))
	 (buffer (get-buffer buffer-name)))
    (if buffer
	(progn
	  (process-send-string buffer (format "cd %s\n" root))
	  (process-send-string buffer "echo ''\n"))
      (message "Buffer %s does not exist.\n" buffer-name))))

(defun nmuth/shell-command-in-directory (dir cmd)
  (let ((buffer-name "*Shell Command Output*"))
    (with-current-buffer-window buffer-name nil nil
				(princ "*------- SHELL COMMAND -------*\n")
				(princ (format "directory:\t%s\n" dir))
				(princ (format "command:\t%s\n" cmd))
				(princ "\n*------- output -------*\n\n")
				(start-process-shell-command "*shell-cmd-process*" buffer-name (format "cd %s && %s" dir cmd))
				(help-mode)
				(switch-to-buffer-other-window (current-buffer)))))

(defun nmuth/shell-command-in-project-root (&optional optional-cmd project-info)
  (interactive)
  (let* ((project (if project-info
		      project-info
		    (nmuth/project-info)))
	 (project-root (plist-get project 'root))
	 (command (if optional-cmd
		      optional-cmd
		    (read-from-minibuffer "Command: "))))
    (message "running: 'cd %s && %s'" project-root command)
    (nmuth/shell-command-in-directory project-root command)))

(defun nmuth/projectile-term-at-root ()
  (interactive)
  (projectile-run-term (getenv "SHELL")))

(def-projectile-commander-method ?t
  "Open a terminal at project root"
  (nmuth/projectile-term-at-root))

(general-define-key :states '(normal insert visual emacs)
		    :prefix "SPC"
		    :non-normal-prefix "C-c"

		    "ps" 'nmuth/projectile-term-at-root
		    "pc" 'nmuth/shell-command-in-project-root
		    "pC" 'projectile-commander)

(general-define-key :states '(normal visual insert emacs)
		    :keymaps 'shell-mode-map
		    :prefix "SPC"
		    :non-normal-prefix "C-c"
		    "mr" 'nmuth/project-shell-cd-to-root)
