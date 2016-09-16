(defun nmuth/org-mode-hook ()
  (interactive)
  (linum-mode 0)) ; not working for some reason

(add-hook 'org-mode-hook 'nmuth/org-mode-hook)

(message "configuring org mode...")

(setq org-directory "~/org")

(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "STARTED" "REVIEW" "|" "DONE")
	(sequence "IDEA" "RESEARCH" "WRITING" "EDITING" "READY" "|" "PUBLISHED")
	(sequence "WAITING" "|" "CANCELLED" "PHONE" "MEETING")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("NEXT" :foreground "deep sky blue" :weight bold)
	("STARTED" :foreground "tomato2" :weight bold)
	("REVIEW" :foreground "dark violet" :weight bold)
	("DONE" :foreground "forest green" :weight bold)

	("IDEA" :foreground "royal blue" :weight bold)
	("RESEARCH" :foreground "deep sky blue" :weight bold)
	("WRITING" :foreground "tomato2" :weight bold)
	("EDITING" :foreground "dark violet" :weight bold)
	("READY" :foreground "green yellow" :weight bold)
	("PUBLISHED" :foreground "forest green" :weight bold)

	("WAITING" :foreground "red" :weight bold)
	("CANCELLED" :foreground "forest green" :weight bold)
	("MEETING" :foreground "forest green" :weight bold)))

(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (list org-directory))
(setq org-startup-indented t)

(general-define-key :states '(normal visual insert emacs)
		    :keymaps 'org-mode-map
		    :prefix "SPC"
		    :non-normal-prefix "C-c"
		    "m ." 'org-time-stamp
		    "m d" 'org-deadline
		    "m a" 'org-agenda
		    "m s" 'org-schedule
		    "m t" 'org-todo
		    "m o" 'org-open-at-point
		    "m I" 'org-clock-in
		    "m O" 'org-clock-out

		    "m S" '(:ignore t :which-key "subtree")
		    "m Sh" 'org-promote-subtree
		    "m Sl" 'org-demote-subtree)
