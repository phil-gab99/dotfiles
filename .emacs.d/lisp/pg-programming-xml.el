;;; pg-programming-xml.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(defun pg/close-tag ()
  (interactive)
  (insert ">")
  (save-excursion (pg/close-tag-maybe)))

(defun pg/close-tag-maybe ()
  (when (looking-back "<[^/]*>")
    (nxml-finish-element)
    (delete-char -1)))

(defun pg/newline-and-indent-maybe ()
  (interactive)
  (if (not (and (looking-back "<[^/]*>\s*")
                (looking-at "\s*</[^/]*>")))
      (newline)
    (newline)
    (newline)
    (indent-according-to-mode)
    (previous-line)
    (indent-according-to-mode)))

(with-eval-after-load 'nxml-mode
  (define-key nxml-mode-map (kbd ">") #'pg/close-tag)
  (define-key nxml-mode-map (kbd "RET") #'pg/newline-and-indent-maybe))

(provide 'pg-programming-xml)
