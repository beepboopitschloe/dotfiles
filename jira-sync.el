(defmacro printf (format-string &rest args)
	`(print (format ,format-string ,@args)))

(defmacro princf (format-string &rest args)
	`(princ (format ,format-string ,@args)))

(defvar jira-sync/jql "assignee=currentUser() AND resolution = Unresolved ORDER BY updatedDate DESC")

(defvar jira-sync/url "https://jira.corp.code42.com/rest/api/2/search")

(defvar jira-sync/temp-buffer-name "*JIRASYNC*")

(defun jira-sync/get-data-from-response (response)
	(json-read-from-string (nth 1 (split-string response "\n\n"))))

(defun jira-sync/json-get (json &rest keys)
	(let ((result json))
		(dolist (key keys result)
			(if (listp result)
					(setq result (cdr (assoc key result)))
				(setq result nil)))
		result))

(defun jira-sync/format-issue (issue)
	(let* ((key (jira-sync/json-get issue 'key))
				 (summary (jira-sync/json-get issue 'fields 'summary))
				 (link-url (format "https://jira.corp.code42.com/browse/%s" key))
				 (link-title (format "%s %s" key summary)))
		(format "[[%s][%s]]" link-url link-title)))

(defun jira-sync/on-search-success (status)
	(let* ((response (buffer-string))
				 (data (jira-sync/get-data-from-response response))
				 (issues (cdr (assoc 'issues data))))
		(with-output-to-temp-buffer jira-sync/temp-buffer-name
			(princ (mapconcat 'jira-sync/format-issue issues "\n"))
			(switch-to-buffer-other-window jira-sync/temp-buffer-name)
			(org-mode))))

(defun jira-sync/make-filter-query (filter)
	(json-encode `((jql . ,filter))))

(defun jira-sync/search (filter)
	(let ((url-request-method "POST")
				(url-request-extra-headers
				 '(("Authorization" . "Basic bm9haC5tdXRoOiRJbm5lck1va2E=")
					 ("Content-Type" . "application/json")))
				(url-request-data (jira-sync/make-filter-query filter)))
		(url-retrieve jira-sync/url #'jira-sync/on-search-success)))

(jira-sync/search jira-sync/jql)
