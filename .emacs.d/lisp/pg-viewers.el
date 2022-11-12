;;; pg-viewers.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(unless (fboundp 'doc-view-mode)
  (autoload #'doc-view-mode "doc-view" nil t))
(add-to-list 'auto-mode-alist '("\\.djvu$" . doc-view-mode))

(straight-use-package 'elfeed)
(unless (fboundp 'elfeed)
  (autoload #'elfeed "elfeed" nil t))
(with-eval-after-load 'elfeed
  (pg/customize-set-variables
   '((elfeed-feeds . ("https://www.gnu.org/software/guile/news/feed.xml"
                      "https://planet.emacslife.com/atom.xml"
                      "https://oneofus.la/have-emacs-will-hack/feed.xml"
                      "http://oremacs.com/atom.xml"
                      "https://updates.orgmode.org/feed/changes"
                      "http://pragmaticemacs.com/feed/"
                      "https://www.reddit.com/r/GUIX.rss"
                      "https://www.reddit.com/r/emacs.rss"
                      "https://www.reddit.com/r/orgmode.rss"
                      "https://blog.tecosaur.com/tmio/rss.xml"))
     (elfeed-search-filter . "@6-months-ago"))))

(unless pg/is-guix-system
  (straight-use-package 'pdf-tools))
(unless (fboundp 'pdf-view-mode)
  (autoload #'pdf-view-mode "pdf-tools" nil t))
(add-to-list 'auto-mode-alist '("\\.pdf$" . pdf-view-mode))

(straight-use-package 'djvu)
(require 'djvu)

(provide 'pg-viewers)
