;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; required packages
(require 'flymake-cursor) ;;display error-messages without mouse
(require 'ibuffer)
(require 'switch-window)
