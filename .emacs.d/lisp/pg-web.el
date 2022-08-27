(defun pg/eww-mode-setup ()
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq-local face-remapping-alist '((variable-pitch (:height 2.0) variable-pitch)
                                     (fixed-pitch (:height 2.0) fixed-pitch)
                                     (default (:height 2.0) default))))

(use-package eww
  :straight nil
  :hook
  (eww-mode . pg/eww-mode-setup))

(provide 'pg-web)
