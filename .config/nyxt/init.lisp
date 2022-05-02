(in-package #:nyxt-user)

(defvar *request-resource-handlers* nil
  "The list of handlers to add to `request-resource-hook'.
These handlers are usually used to block/redirect the requests.")

(dolist (file
	 (list (nyxt-init-file "evil-mode.lisp")
	       (nyxt-init-file "status.lisp")
	       (nyxt-init-file "adblock.lisp")
	       (nyxt-init-file "style.lisp")))
  (load file))

(load-after-system :nx-search-engines (nyxt-init-file "search-engines.lisp"))
(load-after-system :nx-freestance-handler (nyxt-init-file "freestance.lisp"))
;; (load-after-system :nx-dark-reader (nyxt-init-file "dark-reader.lisp"))

(define-configuration web-buffer
    ((request-resource-hook
      (reduce #'hooks:add-hook
	      (mapcar #'make-handler-resource
		      *request-resource-handlers*)
	      :initial-value %slot-default%))))

(define-configuration browser
  ;; This is for Nyxt to never prompt me about restoring the previous session.
    ((session-restore-prompt :never-restore)
     (external-editor-program (list "emacsclient" "-c"))))

;;; Those are settings that every type of buffer should share
(define-configuration (buffer web-buffer internal-buffer editor-buffer prompt-buffer)
  ((default-modes (append '(evil-normal-mode) %slot-default%))
   (download-engine :renderer)
   (current-zoom-ratio 1.25)))

(define-configuration (prompt-buffer)
    ((default-modes (append '(evil-insert-mode) %slot-default%))))

(define-configuration buffer
  ((default-new-buffer-url "https://duckduckgo.com")))
