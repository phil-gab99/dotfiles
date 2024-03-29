(in-package #:nyxt-user)

;; * Evil normal
(define-mode evil-normal-mode ()
  "Enable evil style modal bindings (normal mode).
To enable these bindings by default, add the mode to the list of default modes
in your configuration file.

Example:

\(define-configuration buffer
  ((default-modes (append '(evil-normal-mode) %slot-default%))))

In `evil-insert-mode', CUA bindings are still available unless
`application-mode-p' is non-nil in `evil-insert-mode'.
You can also enable `application-mode' manually to forward all keybindings to
the web page.

See also `evil-insert-mode'."
  ((glyph "N")
   (previous-keymap-scheme-name
    nil
    :type (or keymap:scheme-name null)
    :documentation "The previous keymap scheme that will be used when ending
    normal-mode")
   (keymap-scheme
    (define-scheme "evil-normal"
      scheme:vi-normal
      '("i" evil-insert-mode
        "button1" evil-button1)))
   (destructor
    (lambda (mode)
      (setf (keymap-scheme-name (buffer mode))
            (previous-keymap-scheme-name mode))
       (setf (forward-input-events-p (buffer mode)) t)))
   (constructor
    (lambda (mode)
      (with-accessors
       ((buffer buffer)) mode
       (let ((evil-insert (find-submode buffer 'evil-insert-mode)))
         (setf (previous-keymap-scheme-name mode)
      	 (if evil-insert
      	     (previous-keymap-scheme-name evil-insert)
      	   (keymap-scheme-name buffer))))
       ;; Destroy evil-normal mode after setting previous-keymap-scheme-name,
       ;; or else we can't save the previous keymap scheme.
       (evil-insert-mode :activate nil :buffer buffer)
       (setf (keymap-scheme-name buffer) scheme:vi-normal)
       (setf (forward-input-events-p buffer) nil))))))

;; * Evil insert
(define-mode evil-insert-mode ()
  "Enable evil-insert-style modal bindings (insert mode).
See `evil-normal-mode'."
  ((glyph "I")
   (rememberable-p nil)
   (previous-keymap-scheme-name nil
    :type (or keymap:scheme-name null)
    :documentation "The previous keymap scheme that will be used when ending
vi-normal-mode.")
   (previous-evil-normal-mode nil
    :type (or evil-normal-mode null)
    :documentation "The `evil-normal-mode' that this insert mode is tied to.")
   (keymap-scheme
    (define-scheme "evil-insert"
      scheme:vi-insert
      '("button1" evil-button1
        "C-g" nyxt/prompt-buffer-mode:cancel-input
        "escape" switch-to-evil-normal-mode)))
   (destructor
    (lambda (mode)
      (setf (keymap-scheme-name (buffer mode))
            (previous-keymap-scheme-name mode))))
   (constructor
    (lambda (mode)
      (with-accessors ((buffer buffer)) mode
        (let ((evil-normal (find-submode buffer 'evil-normal-mode)))
          (setf (previous-keymap-scheme-name mode)
      	  (if evil-normal
      	      (previous-keymap-scheme-name evil-normal)
      	      (keymap-scheme-name buffer))
      	  (previous-evil-normal-mode mode)
      	  evil-normal))
        (evil-normal-mode :activate nil :buffer buffer)
        (setf (keymap-scheme-name buffer) scheme:vi-insert))))))


;; * Insert to normal
(define-command switch-to-evil-normal-mode
    (&optional (mode
      	  (find-submode (or (current-prompt-buffer) (current-buffer))
      			'evil-insert-mode)))
  "Switch to the mode remembered to be the matching evil-normal one for this
  MODE. See also `evil-normal-mode' and `evil-insert-mode'."
  (when mode
    (enable-modes (list (or (and (previous-evil-normal-mode mode)
      			   (mode-name (previous-evil-normal-mode mode)))
      		      'evil-normal-mode))
      	    (buffer mode))))

(define-command evil-button1
    (&optional (buffer (or (current-prompt-buffer)
      		     (current-buffer))))
  "Enable evil insert mode when focus is on an input element on the web page.
See also `evil-normal-mode' and `evil-insert-mode'."
  (forward-to-renderer :window (current-window) :buffer buffer)
  (let ((response (nyxt/web-mode:%clicked-in-input? buffer)))
    (cond
      ((and (nyxt/web-mode:input-tag-p response)
            (find-submode buffer 'evil-normal-mode))
       (evil-insert-mode))
      ((and (not (nyxt/web-mode:input-tag-p response))
            (find-submode buffer 'evil-insert-mode))
       (evil-normal-mode)))))

(define-configuration base-mode
    ((keymap-scheme
      (define-scheme "evil-base"
        scheme:vi-normal
        '("C-x C-c" quit
          "C-x C-f" open-file
          "[" switch-buffer-previous
          "]" switch-buffer-next
          "M-x" execute-command
          "C-x k" delete-buffer
          "C-x b" switch-buffer
          "g r" reload-current-buffer
          "o" set-url
          "O" set-url-new-buffer
          "m u" bookmark-url
          "m d" delete-bookmark
          "m l" list-bookmarks
          "y u" copy-url
          "y t" copy-title
          "C-h h" help
          "C-h C" describe-class
          "C-h b" describe-bindings
          "C-h c" describe-command
          "C-h f" describe-function
          "C-h k" describe-key
          "C-h r" manual
          "C-h s" describe-slot
          "C-h t" tutorial
          "C-h v" describe-variable
          "w w" make-window
          "w q" delete-current-window
          "v" nyxt/visual-mode:visual-mode
          "u" reopen-buffer)))))

(define-configuration prompt-buffer
    ((keymap-scheme
      (define-scheme "evil-prompt"
        scheme:vi-normal
        '("C-j" select-next
          "C-k" select-previous
          "C-g" cancel-input)))))

(define-configuration nyxt/web-mode:web-mode
    ((keymap-scheme
      (define-scheme "evil-web"
        scheme:vi-normal
        '("C-s" nyxt/web-mode:search-buffer
          "H" nyxt/web-mode:history-backwards
          "L" nyxt/web-mode:history-forwards
          "y y" nyxt/web-mode:copy
          "p" nyxt/web-mode:paste
          "d d" nyxt/web-mode:cut
          "u" nyxt/web-mode:undo
          "f" nyxt/web-mode:follow-hint
          "F" nyxt/web-mode:follow-hint-new-buffer
          "C-r" nyxt/web-mode:redo
          "m f" nyxt/web-mode:bookmark-hint
          "+" nyxt/web-mode:zoom-page
          "hyphen" nyxt/web-mode:unzoom-page
          "=" nyxt/web-mode:reset-page-zoom
          "j" nyxt/web-mode:scroll-down
          "k" nyxt/web-mode:scroll-up
          "h" nyxt/web-mode:scroll-left
          "j" nyxt/web-mode:scroll-down
          "k" nyxt/web-mode:scroll-up
          "l" nyxt/web-mode:scroll-right
          "G" nyxt/web-mode:scroll-to-bottom
          "g g" nyxt/web-mode:scroll-to-top
          "C-d" nyxt/web-mode:scroll-page-down
          "C-u" nyxt/web-mode:scroll-page-up)))))

(define-configuration nyxt/visual-mode:visual-mode
  ((keymap-scheme
    (define-scheme "evil-visual"
      scheme:vi-normal
      '("h" nyxt/visual-mode:backward-char
        "j" nyxt/visual-mode:forward-line
        "k" nyxt/visual-mode:backward-line
        "l" nyxt/visual-mode:forward-char
        "w" nyxt/visual-mode:forward-word
        "e" nyxt/visual-mode:forward-word
        "b" nyxt/visual-mode:backward-word
        "$" nyxt/visual-mode:end-line
        ")" nyxt/visual-mode:forward-sentence
        "(" nyxt/visual-mode:backward-sentence
        "}" nyxt/visual-mode:forward-paragraph
        "{" nyxt/visual-mode:backward-paragraph
        "C-u" nyxt/visual-mode:forward-document
        "C-d" nyxt/visual-mode:backward-document
        "0" nyxt/visual-mode:beginning-line
        "C-g" nyxt/visual-mode:toggle-mark
        "C-c" nyxt/visual-mode:visual-mode)))))
