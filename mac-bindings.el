;; DESCRIPTION: Mac-friendly key bindings in addition to those provided by Emacs.app
;;              
;; AUTHOR:      Geoffrey Grosenbach
;;              http://peepcode.com
;;
;; DATE:        December 2008 and following
;;
;; NOTE super is the Apple/Command key

; Switch window focus
(global-set-key [(super meta right)] 'other-window)
; TODO Previous window
(global-set-key [(super meta left)] 'other-window)

; Switch buffers in current window
(global-set-key [(super control right)] 'next-buffer)
(global-set-key [(super control left)] 'previous-buffer)

(provide 'mac-bindings)
