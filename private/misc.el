(setq shift-select-mode nil
      mouse-yank-at-point t
      require-final-newline t
      uniquify-buffer-name-style 'forward
      xterm-mouse-mode t
      save-place-file (concat dotfiles-dir "places"))

;; Transparently open compressed files
(auto-compression-mode t)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(set-default 'imenu-auto-rescan t)

(random t) ;; Seed the random-number generator

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)
