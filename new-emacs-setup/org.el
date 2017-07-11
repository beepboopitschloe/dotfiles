(message "configuring org mode...")

(defun nmuth/org-mode-hook ()
  (interactive)
  (linum-mode 0)) ; not working for some reason

(defun nmuth/org-push ()
  "Push org files to git repository."
  (interactive)
  (shell-command "cd ~/org && git add -A && git commit -m \"update $(date)\" && git push"))

(defun nmuth/org-fetch ()
  "Pull org files from git repository."
  (interactive)
  (shell-command "cd ~/org && git pull"))

(defun nmuth/org-clocktable-indent-string (level)
  "Indent an 'org-mode' clocktable without using wacky Latex characters.

LEVEL: number of spaces to offset the string."
  (if (= level 1)
      ""
    (let ((str " "))
      (while (> level 1)
	(setq level (1- level))
	(setq str (concat str "-")))
      (concat str "> "))))

(add-hook 'org-mode-hook 'nmuth/org-mode-hook)

(setq org-directory "~/org")

(setq org-todo-keywords
      '(;; action items
        (sequence "TODO" "NEXT" "STARTED" "REVIEW" "|" "DONE")

        ;; blog posts
        (sequence "IDEA" "RESEARCH" "WRITING" "EDITING" "READY" "|" "PUBLISHED")

        ;; shopping lists
        (sequence "MAYBE" "LISTED" "BUYNEXT" "|" "PURCHASED")

	;; books
	(sequence "OWNED" "READNEXT" "READING" "|" "DONEREADING")

        ;; misc
        (sequence "WAITING" "|" "CANCELLED" "PHONE" "MEETING")))

(setq org-todo-keyword-faces
      '(;; action items
        ("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "deep sky blue" :weight bold)
        ("STARTED" :foreground "tomato2" :weight bold)
        ("REVIEW" :foreground "dark violet" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)

        ;; shopping lists
        ("MAYBE" :foreground "red" :weight bold)
        ("LISTED" :foreground "deep sky blue" :weight bold)
        ("BUYNEXT" :foreground "dark violet" :weight bold)
        ("PURCHASED" :foreground "forest green" :weight bold)

	;; books
        ("OWNED" :foreground "red" :weight bold)
        ("READNEXT" :foreground "deep sky blue" :weight bold)
        ("READING" :foreground "tomato2" :weight bold)
        ("DONEREADING" :foreground "forest green" :weight bold)

        ;; blog posts
        ("IDEA" :foreground "royal blue" :weight bold)
        ("RESEARCH" :foreground "deep sky blue" :weight bold)
        ("WRITING" :foreground "tomato2" :weight bold)
        ("EDITING" :foreground "dark violet" :weight bold)
        ("READY" :foreground "green yellow" :weight bold)
        ("PUBLISHED" :foreground "forest green" :weight bold)

        ;; misc
        ("WAITING" :foreground "red" :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)
        ("MEETING" :foreground "forest green" :weight bold)))

(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (list org-directory))
(setq org-startup-indented t)

(advice-add 'org-clocktable-indent-string :override #'nmuth/org-clocktable-indent-string)

(general-define-key :states '(normal visual insert emacs)
                    :keymaps 'org-mode-map
                    :prefix "SPC"
                    :non-normal-prefix "C-c"
                    "m ." 'org-time-stamp
                    "m a" 'org-agenda
		    "m C" 'org-clock-update-time-maybe
                    "m d" 'org-deadline
		    "m D" 'org-update-all-dblocks

                    "m P" 'nmuth/org-push
                    "m F" 'nmuth/org-fetch

                    "m I" 'org-clock-in
                    "m o" 'org-open-at-point
                    "m O" 'org-clock-out
		    "m q" 'org-fill-paragraph
                    "m s" 'org-schedule
                    "m t" 'org-todo

                    "m S" '(:ignore t :which-key "subtree")
                    "m Sh" 'org-promote-subtree
                    "m Sl" 'org-demote-subtree)

(general-define-key :states '(normal)
                    :keymaps 'org-mode-map
                    "t" 'org-todo
		    "C-i" 'evil-toggle-fold)

(general-define-key :states '(normal visual insert emacs)
		    :keymaps 'org-mode-map

		    "C-c l" 'org-store-link)

(general-define-key :states '(normal visual insert emacs)
                    :keymaps 'org-agenda-mode-map
                    "j" 'evil-next-line
                    "k" 'evil-previous-line)
