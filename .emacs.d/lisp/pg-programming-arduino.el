;;; pg-programming-arduino.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'arduino-mode)

(defun pg/arduino-serial-monitor ()
  (interactive)
  (switch-to-buffer-other-window (buffer-name))
  (arduino-serial-monitor "/dev/ttyACM0" 9600))

(unless (fboundp 'flycheck-arduino-setup)
  (autoload #'flycheck-arduino-setup "flycheck-arduino" nil t))
(add-hook 'arduino-mode-hook #'flycheck-arduino-setup)
(with-eval-after-load 'arduino-mode
  (pg/customize-set-variables
   `((arduino-executable . ,(expand-file-name "~/bin/arduino-flat"))
     (arduino-mode-home . "~/Projects/Arduino/")))
  (define-key arduino-mode-map (kbd "C-c RET") #'pg/arduino-serial-monitor))

(provide 'pg-programming-arduino)
