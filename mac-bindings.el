;; DESCRIPTION: Mac-friendly key bindings in addition to those provided by Emacs.app
;;              
;; AUTHOR:      Geoffrey Grosenbach
;;              http://peepcode.com
;;
;; DATE:        December 2008 and following
;;
;; NOTE super is the Apple/Command key on my keyboard,
;;      but I have cmd/opt switched in System Preferences
;;      so it may be different on your machine).

;; Switch buffers in current window
(global-set-key [(super control right)] 'next-buffer)
(global-set-key [(super control left)] 'previous-buffer)

;; Windows
(global-set-key [f5] 'split-window-horizontally)
(global-set-key [f6] 'split-window-vertically)
(global-set-key [f7] 'delete-window)

(provide 'mac-bindings)
