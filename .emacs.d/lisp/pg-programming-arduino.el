;;; pg-programming-arduino.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'arduino-mode)

(unless (fboundp 'flycheck-arduino-setup)
  (autoload #'flycheck-arduino-setup "flycheck-arduino" nil t))
(add-hook 'arduino-mode-hook #'flycheck-arduino-setup)
(with-eval-after-load 'arduino-mode
  (pg/customize-set-variables
   `((arduino-executable . ,(expand-file-name "~/bin/arduino-flat"))
     (arduino-mode-home . "~/Projects/Arduino/")))
  (require 'ede-arduino))
(with-eval-after-load 'ede-arduino
  (customize-set-variable 'ede-arduino-arduino-command (expand-file-name "~/bin/arduino-flat")))

(provide 'pg-programming-arduino)
