;; Setup
(add-to-list 'load-path (concat dotfiles-dir "/vendor/emacs-jabber-0.8.0"))
(require 'jabber-autoloads)

;; Accounts
(setq jabber-account-list
      '(("yves.senn@gmail.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))
