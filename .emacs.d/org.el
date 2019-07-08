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

(defvar nmuth/target-jira-host "https://jira.target.com")
(defcustom nmuth/default-jira-host nmuth/target-jira-host
  "The default JIRA host to use when inserting links."
  :group 'nmuth
  :type 'variable)

(defun nmuth/is-url-p (str)
  (or (string-prefix-p "http://" str) (string-prefix-p "https://" str)))

(defun nmuth/ticket-number-from-jira-url (url)
  (let* ((parts (split-string url "/"))
         (last-part (car (last parts))))
    last-part))

(defun nmuth/org-link (url label)
  (format "[[%s][%s]]" url (or label url)))

(defun nmuth/jira-org-link-from-string (ticket-or-url)
  (let* ((is-url (nmuth/is-url-p ticket-or-url))
         (ticket-number (if is-url (nmuth/ticket-number-from-jira-url ticket-or-url) ticket-or-url))
         (url (if is-url ticket-or-url (format "%s/browse/%s" nmuth/default-jira-host ticket-number))))
    (nmuth/org-link url ticket-number)))

(defun nmuth/add-jira-link-to-properties (ticket-or-url)
  (interactive "sIssue number or URL: ")
  (let* ((key "JIRA_TICKET")
	 (link (nmuth/jira-org-link-from-string ticket-or-url))
	 (current-value (org-entry-get nil key))
	 (next-value (if current-value (format "%s %s" current-value link) link)))
    (org-set-property key next-value)))

(defun nmuth/insert-jira-link (ticket-or-url)
  (interactive "sIssue number or URL: ")
  (let* ((org-link (nmuth/jira-org-link-from-string ticket-or-url)))
    (insert org-link)))

(defun nmuth/pp-github-issue-or-pr (raw-url)
  (let* ((url (url-generic-parse-url raw-url))
         (parts (split-string (url-filename url) "/"))
         (org (nth 1 parts))
         (repo (nth 2 parts))
         (entity-type (nth 3 parts))
         (entity-id (nth 4 parts)))
    (format "%s/%s#%s" org repo entity-id)))

(defun nmuth/github-issue-or-pr-to-org-link (raw-url)
  (nmuth/org-link raw-url (nmuth/pp-github-issue-or-pr raw-url)))

(defun nmuth/insert-github-issue-or-pr-link (raw-url)
  (interactive "sURL for issue or pull request: ")
  (insert (nmuth/github-issue-or-pr-to-org-link raw-url)))

(defun nmuth/add-pull-request-to-properties (raw-url)
  (interactive "sPull request URL: ")
  (let* ((link (nmuth/github-issue-or-pr-to-org-link raw-url))
         (current-value (org-entry-get (point) "PULL_REQUESTS"))
         (next-value (if current-value (format "%s %s" current-value link) link)))
    (org-entry-put (point) "PULL_REQUESTS" next-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions for copying github link to the current line

(defun nmuth/git-origin ()
  (string-trim (shell-command-to-string "git remote -v | head -n 1 | awk '{ print $2 }'")))

(defun nmuth/git-current-branch ()
  (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))

(defun nmuth/git-ssh-origin-to-url (ssh)
  (let* ((without-colon (replace-regexp-in-string ":" "/" ssh))
         (without-suffix (replace-regexp-in-string "\.git$" "" without-colon))
         (without-username (replace-regexp-in-string "^git@" "https://" without-suffix)))
    without-username))

(defun nmuth/github-origin-link (origin)
  (if (string-prefix-p "http" origin)
      origin
    (nmuth/git-ssh-origin-to-url origin)))

(defun nmuth/path-to-buffer-file-in-project ()
  (replace-regexp-in-string (projectile-project-root) "" buffer-file-name))

(defun nmuth/get-github-link-to-point ()
  (let* ((origin (nmuth/git-origin))
         (branch (nmuth/git-current-branch))
         (base-url (nmuth/github-origin-link origin))
         (branch-url (format "%s/blob/%s" base-url branch))
         (filepath (nmuth/path-to-buffer-file-in-project))
         (line-number (line-number-at-pos)))
    (format "%s/%s\#L%d" branch-url filepath line-number)))

(defun nmuth/copy-github-link-to-point ()
  (interactive)
  (let* ((url (nmuth/get-github-link-to-point))
         (filepath (nmuth/path-to-buffer-file-in-project))
         (line-number (line-number-at-pos))
	 (desc (format "%s#L%s" filepath line-number))
	 (org-link (format "[[%s][%s]]" url desc)))
    (kill-new org-link)))

;; mermaid

(defvar nmuth/mermaid-cmd "~/.config/yarn/global/node_modules/.bin/mmdc")

(defun nmuth/org-mermaid ()
  (interactive)
  (let* ((infile (make-temp-file "org-mermaid"))
         (outfile (make-temp-file "org-mermaid" nil ".png"))
         (el (org-element-at-point))
         (lang (org-element-property :language el))
         (content (org-element-property :value el))
         (cmd (format "%s -i %s -o %s" nmuth/mermaid-cmd infile outfile)))
    (if (string-equal lang "mermaid")
        (with-temp-buffer
          (message cmd)
          (insert content)
          (write-file infile)
          (shell-command cmd)
          (shell-command (format "open %s" outfile)))))
  (message "nmuth/org-mermaid can only be called on :mermaid source blocks"))

(defun nmuth/org-mermaid-share ()
  (interactive)
  (let* ((el (org-element-at-point))
         (lang (org-element-property :language el))
         (content (org-element-property :value el))
         (encoded (base64-encode-string content t))
         (link (format "https://mermaidjs.github.io/mermaid-live-editor/#/view/%s" encoded)))
    (org-open-link-from-string (format "[[%s]]" link))))

(add-hook 'org-mode-hook 'nmuth/org-mode-hook)

(setq org-directory "~/org")

(setq org-todo-keywords
      '(;; action items
        (sequence "TODO" "NEXT" "PROG" "REVW" "|" "DONE(!)")

        (sequence "TASK" "|" "DONE")

        ;; blog posts
        (sequence "IDEA" "RESEARCH" "WRITING" "EDITING" "READY" "|" "PUBLISHED")

        ;; shopping lists
        (sequence "MAYBE" "LISTED" "BUYNEXT" "|" "PURCHASED")

        ;; books
        (sequence "OWNED" "READNEXT" "READING" "|" "DONEREADING")

        ;; bug tracking
        (sequence "REPORTED" "FIXING" "|" "FIXED")

        ;; misc
        (sequence "WAITING" "|" "CANCELLED" "PHONE" "MEETING")))

(setq org-todo-keyword-faces
      '(;; action items
        ("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "deep sky blue" :weight bold)
        ("PROG" :foreground "tomato2" :weight bold)
        ("REVW" :foreground "dark violet" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)

        ;; action item assigned to someone else
        ("TASK" :foreground "deep sky blue" :weight bold)

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

        ;; bug tracking
        ("REPORTED" :foreground "red" :weight bold)
        ("FIXING" :foreground "deep sky blue" :weight bold)
        ("FIXED" :foreground "forest green" :weight bold)

        ;; misc
        ("WAITING" :foreground "red" :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)
        ("MEETING" :foreground "forest green" :weight bold)))

(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (list org-directory))
(setq org-startup-indented t)

(setq org-capture-templates
      '(("t" "Task" entry (file+headline "~/org/planner.org" "tasks")
         "* TODO %?\n%i\n%a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?")))

(defun nmuth/capture-journal ()
  (interactive)
  (org-capture nil "j"))

(advice-add 'org-clocktable-indent-string :override #'nmuth/org-clocktable-indent-string)

;; global org keybindings
(general-define-key :states '(normal visual insert emacs)
                    :prefix "SPC"
                    :non-normal-prefix "C-c"

                    "o" '(:ignore t :which-key "org")
                    "o a" 'org-agenda
                    "o c" 'org-capture
                    "o j" 'nmuth/capture-journal
                    "o O" 'org-clock-out
                    "o l" 'org-store-link
                    "o L" 'nmuth/copy-github-link-to-point
                    "o J" 'nmuth/insert-jira-link
                    "o G" 'nmuth/insert-github-issue-or-pr-link)

;; org-mode specific keybindings
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
                    "m m m" 'nmuth/org-mermaid
                    "m m s" 'nmuth/org-mermaid-share
                    "m o" 'org-open-at-point
                    "m O" 'org-clock-out
                    "m q" 'org-fill-paragraph

                    "m p" '(:ignore t :which-key "heading properties")
                    "m p j" 'nmuth/add-jira-link-to-properties
                    "m p g" 'nmuth/add-pull-request-to-properties

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
