(setq load-path (cons  "/opt/homebrew/lib/erlang/lib/tools-2.6.5.1/emacs"
                       load-path))
(setq erlang-root-dir "/opt/homebrew/lib/erlang")
(setq exec-path (cons "/opt/homebrew/lib/erlang/bin" exec-path))

(add-hook 'erlang-mode-hook
          (lambda ()
            (define-key erlang-mode-map (kbd "C-c C-l") 'erlang-compile)
            ))

(require 'erlang-start)
