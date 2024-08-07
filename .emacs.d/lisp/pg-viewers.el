;;; pg-viewers.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'djvu)
(require 'djvu)

(unless (fboundp 'doc-view-mode)
  (autoload #'doc-view-mode "doc-view" nil t))
(add-hook 'doc-view-mode-hook #'(lambda ()
                                  (display-line-numbers-mode 0)))

(straight-use-package 'elfeed)
(unless (fboundp 'elfeed)
  (autoload #'elfeed "elfeed" nil t))
(add-hook 'elfeed-search-mode-hook #'(lambda ()
                                       (display-line-numbers-mode 0)))
(add-hook 'elfeed-show-mode-hook #'(lambda ()
                                     (display-line-numbers-mode 0)))
(with-eval-after-load 'elfeed
  (setopt elfeed-feeds '("https://www.gnu.org/software/guile/news/feed.xml"
                         "https://planet.emacslife.com/atom.xml"
                         "https://oneofus.la/have-emacs-will-hack/feed.xml"
                         "http://oremacs.com/atom.xml"
                         "https://updates.orgmode.org/feed/changes"
                         "http://pragmaticemacs.com/feed/"
                         "https://www.reddit.com/r/GUIX.rss"
                         "https://www.reddit.com/r/emacs.rss"
                         "https://www.reddit.com/r/orgmode.rss"
                         "https://blog.tecosaur.com/tmio/rss.xml")
          elfeed-search-filter "@6-months-ago"))

(straight-use-package 'nov)
(unless (fboundp 'nov-mode)
  (autoload #'nov-mode "nov" nil t))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(add-hook 'nov-mode-hook #'(lambda ()
                             (display-line-numbers-mode 0)))
(defun pg/nov-mode-setup ()
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq-local face-remapping-alist '((default (:height 1.5) default))))
(add-hook 'nov-mode-hook #'pg/nov-mode-setup)

(unless (fboundp 'pdf-view-mode)
  (autoload #'pdf-view-mode "pdf-tools" nil t))
(add-hook 'pdf-view-mode-hook #'(lambda ()
                                  (display-line-numbers-mode 0)))
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

(provide 'pg-viewers)
