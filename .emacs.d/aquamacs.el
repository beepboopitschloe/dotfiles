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
    ("~/org/finance.org" "~/org/fulgid/toptal.org" "~/org/inkit.org" "~/org/personal.org" "/Users/rose/org/apartments.org" "/Users/rose/org/babel-scratch.org" "/Users/rose/org/dungeon_world.org" "/Users/rose/org/exercise.org" "/Users/rose/org/index.org" "/Users/rose/org/journal.org" "/Users/rose/org/lisp-dev-site.org" "/Users/rose/org/nomad.org" "/Users/rose/org/planner.org" "/Users/rose/org/projects.org" "/Users/rose/org/reading_list.org" "/Users/rose/org/regionsofruin.org" "/Users/rose/org/secure_coding_notes.org" "/Users/rose/org/test.org" "/Users/rose/org/ucahoot.org" "/Users/rose/org/urban_farming.org" "/Users/rose/org/western-roguelite.org" "/Users/rose/org/windows.org" "/Users/rose/org/year_of_hustle.org" "/Users/rose/org/zuki.org")))
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
