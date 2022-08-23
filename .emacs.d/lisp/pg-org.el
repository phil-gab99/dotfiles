(defun org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the `img' directory with respect to the org-buffer's
  location and insert a link to this file."
  (interactive)
  (setq imgpath (concat (let ((abspath (shell-command-to-string (concat "dirname " buffer-file-name))))
                          (with-temp-buffer
                            (call-process "echo" nil t nil "-n" abspath)
                            (delete-char -1)  ;; delete trailing \n
                            (buffer-string)))
                        "/img/"))
  (if (not (f-dir-p imgpath))
      (make-directory imgpath))
  (setq filename
        (concat
         (make-temp-name
          (concat imgpath
                  (let ((bname (shell-command-to-string (concat "basename -s .org " buffer-file-name))))
                    (with-temp-buffer
                      (call-process "echo" nil t nil "-n" bname)
                      (delete-char -1)  ;; delete trailing \n
                      (buffer-string)))
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_"))) ".png"))
  (call-process "import" nil nil nil filename)
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

;; Insert a file and convert it to an org table
(defun org-csv-to-table (beg end)
  "Insert a file into the current buffer at point, and convert it to an org table."
  (interactive (list (mark) (point)))
  (org-table-convert-region beg end ","))

;; Function for defining some behaviours for the major org-mode
(defun pg/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (diminish org-indent-mode)
  (setq evil-auto-indent nil))

(use-package org
  :straight t
  :commands (org-capture org-agenda)
  :hook (org-mode . pg/org-mode-setup)
  :config
  (set-face-attribute 'org-ellipsis nil :underline nil)
  (setq org-ellipsis " ▾")
  (unless pg/is-termux
    (setq org-agenda-files ; Files considered by org-agenda
          '("~/Documents/Org/Agenda/")))
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-deadline-warning-days 7)
  (setq org-todo-keywords ; Defines a new sequence for TODOs, can add more sequences
        '((sequence "TODO(t)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w)" "HOLD(h)" "|"
                    "COMPLETED(c)" "CANC(k)")))

  (unless pg/is-termux
    (setq org-agenda-custom-commands ; Custom org-agenda commands
          '(("d" "Dashboard"
             ((agenda "" ((org-deadline-warning-days 7)))
              (todo "TODO"
                    ((org-agenda-overriding-header "Tasks")))
              (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Tasks")))))

            ("Z" "TODOs"
             ((todo "TODO"
                    ((org-agenda-overriding-header "Todos")))))

            ("m" "Misc" tags-todo "other")

            ("s" "Schedule" agenda ""
             ((org-agenda-files '("~/Documents/Org/Agenda/Schedule-S5-summer.org")))
             ("~/Documents/Schedule-S5-summer.pdf"))

            ("w" "Work Status"
             ((todo "WAIT"
                    ((org-agenda-overriding-header "Waiting")
                     (org-agenda-files org-agenda-files)))
              (todo "REVIEW"
                    ((org-agenda-overriding-header "In Review")
                     (org-agenda-files org-agenda-files)))
              (todo "HOLD"
                    ((org-agenda-overriding-header "On Hold")
                     (org-agenda-todo-list-sublevels nil)
                     (org-agenda-files org-agenda-files)))
              (todo "ACTIVE"
                    ((org-agenda-overriding-header "Active")
                     (org-agenda-files org-agenda-files)))
              (todo "COMPLETED"
                    ((org-agenda-overriding-header "Completed")
                     (org-agenda-files org-agenda-files)))
              (todo "CANC"
                    ((org-agenda-overriding-header "Cancelled")
                     (org-agenda-files org-agenda-files))))))))

  (unless pg/is-termux
    (setq org-capture-templates
          `(("t" "Tasks / Projects")
            ("tt" "Task" entry (file+olp "~/Documents/Org/Agenda/Tasks.org" "Active")
             "* TODO %?\n  DEADLINE: %U\n  %a\n  %i" :empty-lines 1)

            ("j" "Meetings")
            ("jm" "Meeting" entry (file+olp "~/Documents/Org/Agenda/Tasks.org" "Waiting")
             "* TODO %? \n SCHEDULED: %U\n" :empty-lines 1)

            ("m" "Email Workflow")
            ("mr" "Follow Up" entry (file+olp "~/Documents/Org/Agenda/Mail.org" "Follow up")
             "* TODO %a\nDEADLINE: %U%?\n %i" :empty-lines 1))))

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  (setq org-agenda-exporter-settings
        '((ps-left-header (list 'org-agenda-write-buffer-name))
          (ps-right-header
           (list "/pagenumberstring load"
                 (lambda () (format-time-string "%d/%m/%Y"))))
          (ps-font-size '(12 . 11))       ; Lanscape . Portrait
          (ps-top-margin 55)
          (ps-left-margin 35)
          (ps-right-margin 30)))
  (unless pg/is-termux
    (setq org-plantuml-jar-path "~/bin/plantuml.jar"))
  :custom
  (org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                          (vm-imap . vm-visit-imap-folder-other-frame)
                          (gnus . org-gnus-no-new-news)
                          (file . find-file)
                          (wl . wl-other-frame))))

(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode))

(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun pg/diminish-all ()
  (diminish 'which-key-mode)
  (diminish 'org-indent-mode)
  (diminish 'auto-revert-mode)
  (diminish 'buffer-face-mode)
  (diminish 'visual-line-mode))

(defun pg/presentation-setup ()
  (org-display-inline-images)
  (pg/diminish-all)
  (setq-local doom-modeline-minor-modes t)
  (setq-local org-format-latex-options (plist-put org-format-latex-options :scale 2.5))
  (setq-local face-remapping-alist '((default (:height 1.25) default)
                                     (header-line (:height 4.5) variable-pitch)
                                     (variable-pitch (:height 1.25) variable-pitch)
                                     (org-table (:height 1.5) org-table)
                                     (org-verbatim (:height 1.5) org-verbatim)
                                     (org-code (:height 1.5) org-code)
                                     (org-block (:height 1.5) org-block)))
  (variable-pitch-mode 1))

(defun pg/presentation-end ()
  (variable-pitch-mode 0)
  (setq-local doom-modeline-minor-modes nil)
  (setq-local org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (org-latex-preview)
  (setq-local face-remapping-alist '((default variable-pitch default))))

(use-package org-tree-slide
  :straight t
  :hook (((org-tree-slide-before-move-next org-tree-slide-before-move-previous) . org-latex-preview)
         (org-tree-slide-play . pg/presentation-setup)
         (org-tree-slide-stop . pg/presentation-end))
  :after org
  :bind*
  (:map org-tree-slide-mode-map
        ("C-j" . org-tree-slide-move-next-tree)
        ("C-k" . org-tree-slide-move-previous-tree))
  :config
  ;; (unbind-key "<normal-state> C-j" 'org-mode-map)
  ;; (unbind-key "<normal-state> C-k" 'org-mode-map)
  ;; (unbind-key "C->" 'org-tree-slide-mode-map)
  ;; (unbind-key "C-<" 'org-tree-slide-mode-map)
  :custom
  (org-tree-slide-activate-message "Presentation started")
  (org-tree-slide-deactivate-message "Presentation ended")
  (org-tree-slide-breadcrumbs " > ")
  (org-tree-slide-header t)
  (org-image-actual-width nil))

(use-package ox-reveal
  :straight t
  :custom
  (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (org-reveal-hlevel 1)
  (org-export-headline-levels 6)
  (org-reveal-theme "league"))

(use-package org-notify
  :straight t
  :after org
  :custom
  (user-mail-address "pgabriel999@hotmail.com")
  :config
  (org-notify-start)
  (setq org-notify-map nil)
  (org-notify-add 'default '(:time "1w" :actions -notify/window :period "1h" :duration 5))
  (org-notify-add 'meeting '(:time "1w" :actions -email :period "1d")))

;; (org-notify-add 'appt
;;                 '(:time "-1s" :period "20s" :duration 10 :actions (-message -ding))
;;                 '(:time "15m" :period "2m" :duration 100 :actions -notify)
;;                 '(:time "2h" :period "5m" :actions -message)
;;                 '(:time "3d" :actions -email))
;;

(use-package org-mime
  :straight t
  :disabled)

(setq mail-user-agent 'mu4e-user-agent)
(use-package org-msg
  :straight t
  :after mu4e
  :custom
  (org-msg-options "html-postamble:nil toc:nil author:nil num:nil \\n:t")
  (org-msg-startup "indent inlineimages hidestars")
  (org-msg-greeting-fmt "\nBonjour/Hi %s,\n\n")
  ;; (org-msg-recipient-names '(("user@domain.com" . "Name")))
  (org-msg-greeting-name-limit 3)
  (org-message-convert-citation t)
  (org-msg-signature (concat "\n\nCordialement/Regards,\n\n*--*\n" mu4e-compose-signature))
  (org-msg-recipient-names nil)
  :config
  (org-msg-mode))

(unless pg/is-termux
  (use-package org-roam
    :straight t
    :custom
    (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    (org-roam-directory "~/Documents/Notes")
    (org-roam-capture-templates
     '(("d" "default" plain
        "%?"
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+STARTUP: latexpreview inlineimages\n#+date: %U\n")
        :unnarrowed t)
       ("e" "economy" plain
        "%?"
        :if-new (file+head "ECN-1000/notes/%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: ecn1000-${title}\n#+STARTUP: latexpreview inlineimages\n#+date: %U\n")
        :unnarrowed t)
       ("m" "Morgan Stanley" plain
        "%?"
        :if-new (file+head "MorganStanley/notes/%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: morgan-stanley-${title}\n#+STARTUP: latexpreview inlineimages\n#+date: %U\n")
        :unnarrowed t)))
       :config
       (org-roam-setup)))

(use-package org-fragtog
  :straight t
  :hook (org-mode . org-fragtog-mode))

(font-lock-add-keywords 'org-mode ; Replace '-' with bullets
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region
                                         (match-beginning 1) (match-end 1) "•"))))))

(require 'org-indent) ; Changes some org structures to fixed pitch
(set-face-attribute 'org-block nil :foreground nil :background "gray5" :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :foreground "orange" :inherit 'fixed-pitch)
(set-face-attribute 'org-verbatim nil :foreground "green" :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :foreground "thistle3" :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(dolist (face '((org-level-1 . 1.2) ; Sets font for variable-pitch text
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'regular :height (cdr face)))

(with-eval-after-load 'org ; Defer the body code until org is loaded
  (org-babel-do-load-languages ; Loads languages to be executed by org-babel
   'org-babel-load-languages '((emacs-lisp . t)
                               (java . t)
                               (shell . t)
                               (plantuml . t)
                               ;; (jupyter . t)
                               (python . t)))

  (setq org-confirm-babel-evaluate nil)

  (require 'org-tempo) ; Allows defined snippets to expand into appropriate code blocks
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("java" . "src java"))
  (add-to-list 'org-structure-template-alist '("als" . "src alloy"))
  (add-to-list 'org-structure-template-alist '("puml" . "src plantuml"))
  (add-to-list 'org-structure-template-alist '("vhd" . "src vhdl"))
  (add-to-list 'org-structure-template-alist '("asm" . "src mips"))
  (add-to-list 'org-structure-template-alist '("cc" . "src c"))
  (add-to-list 'org-structure-template-alist '("smv" . "src smv"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sql" . "src sql"))

  (add-to-list 'org-src-lang-modes '("als" . alloy))
  (add-to-list 'org-src-lang-modes '("smv" . nusmv))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(defun pg/org-babel-tangle-config () ; Automatic tangle of emacs config file
  ;; (when (string-equal (file-name-directory (buffer-file-name))
  ;;                     (expand-file-name "~/.emacs.d/"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda ()
                           (add-hook 'after-save-hook #'pg/org-babel-tangle-config)))

(defun pg/timer-setup ()
  "Sets up some parameters for the timer"
  (setq org-clock-sound "~/Misc/ding.wav"))

(defun pg/start-timer ()
  "Begins Pomodoro timer with study timer"
  (interactive)
  (pg/timer-setup)
  (pg/study-timer))

(defun pg/start-with-break-timer ()
  "Begin Pomodoro timer with break timer"
  (interactive)
  (pg/timer-setup)
  (pg/break-timer))

(defun pg/stop-timer ()
  "Stops the timer"
  (interactive)
  (setq org-clock-sound nil)
  (remove-hook 'org-timer-done-hook #'pg/study-timer)
  (remove-hook 'org-timer-done-hook #'pg/break-timer)
  (org-timer-stop))

(defun pg/study-timer ()
  "Study timer for 1 hour"
  (add-hook 'org-timer-done-hook #'pg/break-timer)
  (remove-hook 'org-timer-done-hook #'pg/study-timer)
  (setq org-timer-default-timer "1:00:00")
  (setq current-prefix-arg '(4)) ; Universal argument
  (call-interactively #'org-timer-set-timer))

(defun pg/break-timer ()
  "Break timer for 30 minutes"
  (add-hook 'org-timer-done-hook #'pg/study-timer)
  (remove-hook 'org-timer-done-hook #'pg/break-timer)
  (setq org-timer-default-timer "30:00")
  (setq current-prefix-arg '(4)) ; Universal argument
  (call-interactively #'org-timer-set-timer))

(provide 'pg-org)
