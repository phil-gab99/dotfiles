;; (straight-use-package 'dtk)
;; (with-eval-after-load 'evil-collection
;;   (unless (fboundp 'dtk)
;;     (autoload #'dtk "dtk" nil t))
;;   (with-eval-after-load 'dtk
;;     (evil-collection-define-key 'normal 'dtk-mode-map
;;       (kbd "C-j") 'dtk-forward-verse
;;       (kbd "C-k") 'dtk-backward-verse
;;       (kbd "C-f") 'dtk-forward-chapter
;;       (kbd "C-b") 'dtk-backward-chapter
;;       "q" 'dtk-quit
;;       "c" 'dtk-clear-dtk-buffer
;;       "s" 'dtk-search)
;;     (add-hook 'dtk-mode-hook #'(lambda ()
;;                                  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)))))
;;     (customize-set-variable 'dtk-module "KJV")
;;     (customize-set-variable 'dtk-module-category "Biblical Texts")
;;     (customize-set-variable 'dtk-word-wrap t)))

(provide 'pg-bible)
