;;; pg-programming-arduino.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(unless (or pg/is-termux
            pg/is-windows)
  (straight-use-package 'arduino-mode)

  (defconst sketch-bare-minimum
    (concat "/**\n * @author Philippe Gabriel\n */\n\n"
            "void setup() {\n  // put your setup code here, to run once:\n}\n\n"
            "void loop() {\n  // put your main code here, to run repeatedly:\n}")
    "Arduino sketches bare minimum code.")

  (defun pg/arduino-serial-monitor ()
    "Opens the Arduino Serial Monitor."
    (interactive)
    (switch-to-buffer-other-window (buffer-name))
    (arduino-serial-monitor "/dev/ttyACM0" 9600))

  (defun pg/arduino-sketch-new (sketch)
    "Creates a new Arduino Sketch."
    (interactive (list (read-from-minibuffer "Arduino new sketch file: ")))
    (let* ((sketch-dir (concat arduino-mode-home "Sketches/code/" sketch))
           (sketch-file (concat sketch-dir "/" sketch ".ino")))
      (make-directory sketch-dir)
      (write-region sketch-bare-minimum nil sketch-file nil nil nil t)
      (find-file sketch-file)))

  (unless (fboundp 'flycheck-arduino-setup)
    (autoload #'flycheck-arduino-setup "flycheck-arduino" nil t))
  (add-hook 'arduino-mode-hook #'flycheck-arduino-setup)
  (with-eval-after-load 'arduino-mode
    (pg/customize-set-variables
     `((arduino-executable . ,(expand-file-name "~/bin/arduino-flat"))
       (arduino-mode-home . "~/Projects/Arduino/")))
    (define-key arduino-mode-map (kbd "C-c RET") #'pg/arduino-serial-monitor)))

(provide 'pg-programming-arduino)
