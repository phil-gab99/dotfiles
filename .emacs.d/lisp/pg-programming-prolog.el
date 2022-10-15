(straight-use-package 'prolog)
(unless (fboundp 'prolog-mode)
  (autoload #'prolog-mode "prolog" nil t))
