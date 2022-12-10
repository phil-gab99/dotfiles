;;; pg-programming-arduino.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'arduino-mode)

(unless (fboundp 'flycheck-arduino-setup)
  (autoload #'flycheck-arduino-setup "flycheck-arduino" nil t))
(add-hook 'arduino-mode-hook #'flycheck-arduino-setup)
(with-eval-after-load 'arduino-mode
  (customize-set-variable 'arduino-mode-home "~/Projects/Arduino/"))

(provide 'pg-programming-arduino)
