;; redirect to ~/.emacs.d/init.el

;; keep package happy
;(package-initialize)

(load "~/.emacs.d/init.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7527f3308a83721f9b6d50a36698baaedc79ded9f6d5bd4e9a28a22ab13b3cb1" default)))
 '(mac-command-modifier (quote meta))
 '(org-agenda-files
   (quote
    ("~/org/finance.org" "~/org/fulgid/toptal.org" "~/org/inkit.org" "~/org/personal.org" "/Users/nmuth/org/apartments.org" "/Users/nmuth/org/babel-scratch.org" "/Users/nmuth/org/dungeon_world.org" "/Users/nmuth/org/exercise.org" "/Users/nmuth/org/index.org" "/Users/nmuth/org/journal.org" "/Users/nmuth/org/lisp-dev-site.org" "/Users/nmuth/org/nomad.org" "/Users/nmuth/org/planner.org" "/Users/nmuth/org/projects.org" "/Users/nmuth/org/reading_list.org" "/Users/nmuth/org/regionsofruin.org" "/Users/nmuth/org/secure_coding_notes.org" "/Users/nmuth/org/test.org" "/Users/nmuth/org/ucahoot.org" "/Users/nmuth/org/urban_farming.org" "/Users/nmuth/org/western-roguelite.org" "/Users/nmuth/org/windows.org" "/Users/nmuth/org/year_of_hustle.org" "/Users/nmuth/org/zuki.org")))
 '(package-selected-packages
   (quote
    (persistent-scratch nord-theme slime-company slime-fancy slime lispy less-css-mode cider clojure-mode ponylang-mode vue-mode company-dcd flycheck-dmd-dub alchemist elixir-mode haskell-mode markdown-mode yaml-mode which-key web-mode use-package tide smart-tabs-mode shell-pop restclient restart-emacs racer projectile omnisharp npm-mode nim-mode lua-mode linum-relative ledger-mode go-eldoc general flycheck-rust flycheck-elm flx-ido exec-path-from-shell evil-magit evil-escape emojify elm-mode d-mode company-go color-theme anaconda-mode ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((((type tty)) (:foreground "green"))))
 '(magit-diff-added-highlight ((((type tty)) (:foreground "LimeGreen"))))
 '(magit-diff-context-highlight ((((type tty)) (:foreground "default"))))
 '(magit-diff-file-heading ((((type tty)) nil)))
 '(magit-diff-removed ((((type tty)) (:foreground "red"))))
 '(magit-diff-removed-highlight ((((type tty)) (:foreground "IndianRed"))))
 '(magit-section-highlight ((((type tty)) nil))))
(put 'downcase-region 'disabled nil)
