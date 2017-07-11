(message "configuring golang...")

;; with help from https://github.com/sigma/dotemacs/blob/master/lisp/config/go-config.el

(let* ((home (getenv "HOME"))
       (path (getenv "PATH"))
       (gopath (concat home "/Projects/go"))
       (gobin (concat gopath "/bin")))
  (progn
    (setenv "GOPATH" gopath)
    (setenv "PATH" (concat path ":" gopath))
    (setq exec-path (append exec-path (list gobin)))))

(defun nmuth/find-go-package (pkg)
  (let* ((gopath (getenv "GOPATH"))
         (pkg-path (concat gopath "/src/" pkg)))
    (if (file-exists-p pkg-path)
        pkg-path
      nil)))

(nmuth/find-go-package "golang.org/x/tools/cmd/goimports")

(defun nmuth/ensure-go-package (pkg)
  (or (nmuth/find-go-package pkg)
      (and (progn
             (message "Downloading go package: %s" pkg)
             (shell-command (format "go get -u %s" pkg)))
           (nmuth/find-go-package pkg))))

(defun nmuth/load-file-from-gopath-or-download (pkg file)
  (let ((path (concat (nmuth/ensure-go-package pkg)
                      "/" file)))
    (when (file-exists-p path)
      (load-file path))))

(defun nmuth/go-mode-hook ()
  (interactive)
  (add-hook 'before-save-hook 'gofmt-before-save t)
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode +1)
  (go-eldoc-setup))

(use-package go-eldoc :ensure t)
(use-package company-go :ensure t)
(use-package go-mode
  :config (progn
            (add-hook 'go-mode-hook #'nmuth/go-mode-hook)

            ;; gofmt replacement
            (when (nmuth/ensure-go-package "golang.org/x/tools/cmd/goimports")
              (setq gofmt-command "goimports"))

            ;; for company-go
            (nmuth/ensure-go-package "github.com/nsf/gocode")

            ;; for compilation
            (nmuth/ensure-go-package "github.com/sigma/gocyclo")

            ;; external emacs modules
            (nmuth/load-file-from-gopath-or-download "golang.org/x/tools/cmd/guru"
                                                     "go-guru.el")
            (nmuth/load-file-from-gopath-or-download "github.com/dougm/goflymake"
                                                     "go-flycheck.el")
            (nmuth/load-file-from-gopath-or-download "github.com/golang/lint"
                                                     "misc/emacs/golint.el")
            (require 'go-flycheck))
  :ensure t)

(general-define-key :states '(normal visual insert emacs)
                    :keymaps 'go-mode-map
                    :prefix "SPC"
                    :non-normal-prefix "C-c"

                    )
