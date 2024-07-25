;;; pg-org.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(defun pg/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the img directory
with respect to the org-buffer's location and insert a link to this
file. Requires imageMagick for undertaking screenshots."
  (interactive)
  (setq imgpath "./img/")
  (if (not (f-dir-p imgpath))
      (make-directory imgpath))
  (setq filename
        (concat
         (make-temp-name
          (concat imgpath
                  (let ((bname (string-trim (shell-command-to-string (concat "basename -s .org " buffer-file-name)))))
                    (with-temp-buffer
                      (call-process "echo" nil t nil "-n" bname)
                      (buffer-string)))
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_"))) ".png"))
  (call-process "grimshot" nil nil nil "--notify" "save" "area" filename)
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

(defun pg/org-csv-to-table (beg end)
  "Insert a file into the current buffer at point, and convert it to an org
table."
  (interactive (list (mark) (point)))
  (org-table-convert-region beg end ","))

(defconst pg/org-view-html-tmp-dir "/tmp/org-html-preview/"
  "Directory of temporary html file to view.")

(defun pg/org-view-html ()
  "Views an org source html block in default browser"
  (interactive)
  (let ((elem (org-element-at-point))
        (temp-file-path (concat pg/org-view-html-tmp-dir (number-to-string (random (expt 2 32))) ".html")))
    (cond ((not (eq 'export-block (car elem)))
           (message "Not in an export block!"))
          ((not (string-equal (plist-get (car (cdr elem)) :type) "HTML"))
           (message "Export block is not HTML!"))
          (t (progn (f-mkdir pg/org-view-html-tmp-dir)
                    (f-write (plist-get (car (cdr elem)) :value) 'utf-8 temp-file-path)
                    (start-process "org-html-preview" nil "xdg-open" temp-file-path))))))

(defun pg/org-babel-tangle-config ()
  "Automatic tangle of org files."
  (when (eq major-mode #'org-mode)
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(defun pg/org-mode-setup ()
  "Define some behaviours for the major org-mode."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (display-line-numbers-mode 0)
  (diminish 'org-indent-mode)
  (setq-local evil-auto-indent nil))

(straight-use-package 'org)
(require 'org)
(add-hook 'org-mode-hook #'pg/org-mode-setup)
(add-hook 'org-mode-hook #'(lambda ()
                             (add-hook 'after-save-hook #'pg/org-babel-tangle-config)))
(with-eval-after-load 'org
  (setopt org-ellipsis " ▾"
          org-hide-emphasis-markers t
          org-auto-align-tags nil
          org-tags-column 0
          org-catch-invisible-edits 'show-and-error
          org-special-ctrl-a/e t
          org-insert-heading-respect-content t
          org-pretty-entities t
          org-log-done 'time
          org-fontify-quote-and-verse-blocks t
          org-log-into-drawer t
          org-deadline-warning-days 7
          org-todo-keywords '((sequence "TODO(t)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k)"))
          ;; org-plantuml-jar-path (expand-file-name "~/.guix-home/profile/share/java/plantuml.jar")
          org-babel-python-command "python3"
          org-confirm-babel-evaluate nil
          org-agenda-exporter-settings `((ps-left-header (org-agenda-write-buffer-name))
                                         (ps-right-header ("/pagenumberstring load" ,(lambda ()
                                                                                       (format-time-string "%d/%m/%Y"))))
                                         (ps-font-size (12 . 11))
                                         (ps-top-margin 55)
                                         (ps-left-margin 35)
                                         (ps-right-margin 30)))

  (require 'org-indent)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font (plist-get pg/user :font-variable) :weight 'regular :height (cdr face)))

  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

  (defun pg/babel-ansi ()
    (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
      (save-excursion
        (goto-char beg)
        (when (looking-at org-babel-result-regexp)
          (let ((end (org-babel-result-end))
                (ansi-color-context-region nil))
            (ansi-color-apply-on-region beg end))))))
  (add-hook 'org-babel-after-execute-hook #'pg/babel-ansi)

  (require 'org-tempo) ;; Allows defined snippets to expand into appropriate code blocks
  (dolist (template '(("sh" . "src sh")
                      ("java" . "src java")
                      ("als" . "src alloy")
                      ;; ("puml" . "src plantuml")
                      ("vhd" . "src vhdl")
                      ("asm" . "src mips")
                      ("lmc" . "src lmc-java")
                      ("cc" . "src c")
                      ("el" . "src emacs-lisp")
                      ("hs" . "src haskell")
                      ("py" . "src python")
                      ("jp" . "src jupyter-python")
                      ("sql" . "src sql")
                      ("for" . "src fortran")))
    (add-to-list 'org-structure-template-alist template))

  (dolist (src '(("als" . alloy)
                 ("lmc-java" . lmc-java)
                 ("plantuml" . plantuml)))
    (add-to-list 'org-src-lang-modes src))

  (with-eval-after-load 'general
    (pg/leader-keys
      "o" '(:ignore t :which-key "org")
      "os" '(pg/org-screenshot :which-key "screenshot")
      "oc" '(org-capture :which-key "capture")
      "oa" '(org-agenda :which-key "agenda")
      "ot" '(org-todo-list :which-key "todos")
      "ol" '(:ignore t :which-key "links")
      "olo" '(org-open-at-point :which-key "open")
      "olb" '(org-mark-ring-goto :which-key "back")))

  (unless pg/is-termux
    (setopt org-agenda-files (list (concat (plist-get pg/user :home) "/Sync/Agenda/"))
            org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                                   (vm-imap . vm-visit-imap-folder-other-frame)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file)
                                   (wl . wl-other-frame))
            org-agenda-custom-commands '(("d" "Dashboard"
                                          ((agenda ""
                                                   ((org-deadline-warning-days 7)))
                                           (todo "TODO"
                                                 ((org-agenda-overriding-header "Tasks")))
                                           (tags-todo "agenda/ACTIVE"
                                                      ((org-agenda-overriding-header "Active Tasks")))))

                                         ("Z" "TODOs"
                                          ((todo "TODO"
                                                 ((org-agenda-overriding-header "Todos")))))

                                         ("m" "Misc" tags-todo "other")

                                         ("s" "Schedule" agenda ""
                                          ((org-agenda-files org-agenda-files)))

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
                                                  (org-agenda-files org-agenda-files))))))
            org-capture-templates `(("t" "Tasks / Projects")

                                    ("tt" "Task" entry
                                     (file+olp ,(concat (plist-get pg/user :home) "/Sync/Agenda/Tasks.org") "Active")
                                     "* TODO %? :task:\nDEADLINE: %U\n  %a\n  %i" :empty-lines 1)

                                    ("tr" "Repeat" entry
                                     (file+olp ,(concat (plist-get pg/user :home) "/Sync/Agenda/Tasks.org") "Repeat")
                                     "* TODO %? :task:\n%^{notify|repeat}p" :empty-lines 1)

                                    ("j" "Meetings")
                                    ("jm" "Meeting" entry
                                     (file+olp ,(concat (plist-get pg/user :home) "/Sync/Agenda/Tasks.org") "Waiting")
                                     "* TODO %? \nSCHEDULED: %U\n" :empty-lines 1)

                                    ("m" "Email Workflow")
                                    ("mr" "Follow Up" entry
                                     (file+olp ,(concat (plist-get pg/user :home) "/Sync/Agenda/Tasks.org") "Follow up")
                                     "* TODO %a\nDEADLINE: %U%?\n %i" :empty-lines 1))
            org-format-latex-options (plist-put org-format-latex-options :scale 1.5))))

(add-hook 'org-agenda-mode-hook #'(lambda ()
                                    (display-line-numbers-mode 0)))
(with-eval-after-load 'org-agenda
  (setopt org-agenda-tags-column 0
          org-agenda-block-separator ?─
          org-agenda-time-grid '((daily today require-timed)
                                 (800 1000 1200 1400 1600 1800 2000)
                                 " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
          org-agenda-current-time-string "◀── now ─────────────────────────────────────────────────"))

(straight-use-package 'org-appear)
(unless (fboundp 'org-appear-mode)
  (autoload #'org-appear-mode "org-appear" nil t))
(add-hook 'org-mode-hook #'org-appear-mode)

(straight-use-package 'org-contacts)
(with-eval-after-load 'org-contacts
  (setopt org-contacts-files (list (expand-file-name "~/Sync/Social/contacts.org"))))

(defun pg/presentation-setup ()
  "Setup before starting org presentation."
  (org-display-inline-images)
  (setq-local doom-modeline-minor-modes t
              org-format-latex-options (plist-put org-format-latex-options :scale 2.5)
              face-remapping-alist '((default (:height 1.75) default)))
  (org-latex-preview)
  (variable-pitch-mode 1))

(defun pg/presentation-end ()
  "Cleanup after ending org presentation."
  (variable-pitch-mode 0)
  (setq-local doom-modeline-minor-modes nil
              org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
              face-remapping-alist '((default variable-pitch default)))
  (org-latex-preview))

(straight-use-package 'org-tree-slide)
(unless (fboundp 'org-tree-slide-mode)
  (autoload #'org-tree-slide-mode "org-tree-slide" nil t))
(with-eval-after-load 'general
  (pg/leader-keys
    "op" '(org-tree-slide-mode :which-key "slide")))
(with-eval-after-load 'org-tree-slide
  (dolist (command '(org-tree-slide-before-move-next-hook
                     org-tree-slide-before-move-previous-hook))
    (add-hook command #'org-latex-preview))
  (add-hook 'org-tree-slide-play-hook #'pg/presentation-setup)
  (add-hook 'org-tree-slide-stop-hook #'pg/presentation-end)
  (setopt org-tree-slide-activate-message "Presentation started"
          org-tree-slide-deactivate-message "Presentation ended"
          org-tree-slide-breadcrumbs " > "
          org-tree-slide-header t
          org-image-actual-width nil))

(straight-use-package 'ox-reveal)
(unless (fboundp 'org-reveal-export-to-html)
  (autoload #'org-reveal-export-to-html "ox-reveal" nil t))
(with-eval-after-load 'ox-reveal
  (setopt org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"
          org-reveal-hlevel 1
          org-export-headline-levels 6
          org-reveal-theme "league"))

(straight-use-package 'org-modern)
(with-eval-after-load 'org
  (setopt org-modern-list '((?+ . "○")
                            (?- . "◉")
                            (?* . "▪"))
          org-modern-table nil)
  (global-org-modern-mode))

(unless pg/is-windows
  (straight-use-package 'org-notify)
  (with-eval-after-load 'org
    (require 'org-notify))
  (with-eval-after-load 'org-notify
    (org-notify-start)
    (setq org-notify-map nil)
    (org-notify-add 'default '( :time "1w"
                                :actions (-notify/window)
                                :period "1h"
                                :duration 5))
    (org-notify-add 'repeat '( :time "1w"
                               :actions (-notify/window)
                               :period "1w"
                               :duration 5))))

(straight-use-package 'org-msg)
(unless (fboundp 'org-msg-mode)
  (autoload #'org-msg-mode "org-msg" nil t))
(add-hook 'mu4e-compose-pre-hook #'org-msg-mode)
(with-eval-after-load 'org-msg
  (setopt org-msg-options "html-postamble:nil toc:nil author:nil num:nil \\n:t"
          org-msg-signature (concat "\n\nCordialement/Regards,\n\n*--*\n"
                                    "Philippe Gabriel - 40160338 \n[[mailto:pgabriel999@hotmail.com][pgabriel999@hotmail.com]]")
          org-msg-startup "indent inlineimages hidestars"
          org-msg-greeting-fmt "\nBonjour/Hi %s,\n\n"
          org-msg-greeting-name-limit 3
          org-message-convert-citation t
          org-msg-default-alternatives '((new html)
                                         (reply-to-text html)
                                         (reply-to-html html))
          org-msg-recipient-names nil))

(with-eval-after-load 'org
  (straight-use-package 'org-roam)
  (require 'org-roam))
(with-eval-after-load 'org-roam
  (setopt org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
          org-roam-directory (concat (plist-get pg/user :documents) "/Notes")
          org-roam-capture-templates '(("d" "default" plain
                                        "%?"
                                        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                           "#+title: ${title}\n#+STARTUP: latexpreview inlineimages\n#+date: %U\n")
                                        :unnarrowed t)
                                       ("a" "system admin" plain
                                        "%?"
                                        :if-new (file+head "IFT-3830/notes/%<%Y%m%d%H%M%S>-${slug}.org"
                                                           "#+title: ift3830-${title}\n#+STARTUP: latexpreview inlineimages\n#+date: %U\n")
                                        :unnarrowed t)
                                       ("s" "distributed system design" plain
                                        "%?"
                                        :if-new (file+head "COMP-6231/notes/%<%Y%m%d%H%M%S>-${slug}.org"
                                                           "#+title: comp6231-${title}\n#+STARTUP: latexpreview inlineimages\n#+date: %U\n")
                                        :unnarrowed t)))
  (org-roam-db-autosync-enable)
  (with-eval-after-load 'general
    (pg/leader-keys
      "on" '(:ignore t :which-key "notes")
      "onl" '(org-roam-buffer-toggle :which-key "links")
      "onf" '(org-roam-node-find :which-key "find/create")
      "oni" '(org-roam-node-insert :which-key "insert/create"))))

(straight-use-package 'org-fragtog)
(unless (fboundp 'org-fragtog-mode)
  (autoload #'org-fragtog-mode "org-fragtog" nil t))
(add-hook 'org-mode-hook #'org-fragtog-mode)

(defun pg/timer-setup ()
  "Sets up some parameters for the timer"
  (setq org-clock-sound (expand-file-name "~/Misc/ding.wav")))

(defun pg/start-timer ()
  "Begins Pomodoro timer with study timer"
  (interactive)
  (pg/timer-setup)
  (pg/study-timer))

(defun pg/start-with-break-timer ()
  "Begins Pomodoro timer with break timer"
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
  (setq current-prefix-arg '(4)) ;; Universal argument
  (call-interactively #'org-timer-set-timer))

(defun pg/break-timer ()
  "Break timer for 15 minutes"
  (add-hook 'org-timer-done-hook #'pg/study-timer)
  (remove-hook 'org-timer-done-hook #'pg/break-timer)
  (setq org-timer-default-timer "15:00")
  (setq current-prefix-arg '(4)) ;; Universal argument
  (call-interactively #'org-timer-set-timer))

(with-eval-after-load 'general
  (pg/leader-keys
    "ow" '(:ignore t :which-key "pomodoro")
    "owt" '(pg/start-timer :which-key "start")
    "owb" '(pg/start-with-break-timer :which-key "break")
    "ows" '(pg/stop-timer :which-key "stop")
    "owp" '(org-timer-pause-or-continue :which-key "pause")))

(provide 'pg-org)
