;; (defvar *evil-keymap* (make-keymap "evil-map"))
;; (define-key *evil-keymap*
;;   "M-x" 'execute-command
;;   "C-j" 'nyxt/prompt-buffer-mode:select-next
;;   "C-k" 'nyxt/prompt-buffer-mode:select-previous)

;; (define-mode evil-mode ()
;;   "Mode for integrating vi keybindings to emacs keybindings."
;;   ((keymap-scheme (keymap:make-scheme
;;                    scheme:cua *evil-keymap*
;;                    scheme:emacs *evil-keymap*
;;                    scheme:vi-normal *evil-keymap*))))

;; (define-configuration buffer
;;   ((default-modes (append '(evil-mode) %slot-default%))))
