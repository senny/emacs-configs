;; setup Erlang environment
(setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.6.6/emacs" load-path))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))

;; customizations
(eval-after-load 'erlang
  (add-hook 'erlang-mode-hook
            (lambda ()
              (require 'erlang-flymake)
              (local-set-key (kbd "C-c C-l") 'erlang-compile)
              )))

(require 'erlang-start)
