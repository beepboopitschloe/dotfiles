(defun spacemacs/alternate-buffer ()
  (interactive)
  (if (evil-alternate-buffer)
      (switch-to-buffer (car (evil-alternate-buffer)))
    (switch-to-buffer (other-buffer (current-buffer) t))))

(defun spacemacs/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
	   (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

(defun spacemacs/split-window-right-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
	     (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun spacemacs/split-window-below-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
	     (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun spacemacs/indent-region-or-buffer ()
  (interactive)
  (save-excursion
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indented selected region."))
      (progn
	(evil-indent (point-min) (point-max))
	(message "Indented buffer.")))
    (whitespace-cleanup)))
